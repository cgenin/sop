use std::fmt;
use std::str;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::tag_no_case;
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::opt;
use nom::IResult;
use nom::sequence::tuple;

use crate::table::{bytes_to_string, Table, table};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum MaterializedViewLogOption {
    Preserve,
    Purge,
}

impl<'a> From<&'a str> for MaterializedViewLogOption {
    fn from(t: &str) -> MaterializedViewLogOption {
        let upp = t.to_uppercase();
        let value = upp.as_str();
        match value {
            "PRESERVE" => MaterializedViewLogOption::Preserve,
            "PURGE" => MaterializedViewLogOption::Purge,
            _ => panic!("MaterializedViewLogOption not found {} ", value)
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum StorageOption {
    Drop,
    Reuse,
}

impl<'a> From<&'a str> for StorageOption {
    fn from(t: &str) -> StorageOption {
        let upp = t.to_uppercase();
        let value = upp.as_str();
        match value {
            "DROP" => StorageOption::Drop,
            "REUSE" => StorageOption::Reuse,
            _ => panic!("StorageOption not found {} ", value)
        }
    }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct TruncateTable {
    table: Table,
    materialized: Option<MaterializedViewLogOption>,
}

impl fmt::Display for TruncateTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let materialized_log = self.materialized.clone().map(|m| {
            match m {
                MaterializedViewLogOption::Preserve => "PRESERVE",
                MaterializedViewLogOption::Purge => "PURGE"
            }
        })
            .map(|r| format!(" {} MATERIALIZED VIEW LOG", r))
            .unwrap_or("".to_string());
        write!(f, " TABLE {}{}", self.table, materialized_log)?;
        Ok(())
    }
}


#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct TruncateCluster {
    cluster_name: Table,
}

impl fmt::Display for TruncateCluster {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " CLUSTER {}", self.cluster_name)?;
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum TruncateType {
    Table(TruncateTable),
    Cluster(TruncateCluster),
}

impl fmt::Display for TruncateType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TruncateType::Table(t) => t.fmt(f)?,
            TruncateType::Cluster(t) => t.fmt(f)?,
        }
        Ok(())
    }
}

fn truncate_table_type(i: &[u8]) -> IResult<&[u8], TruncateType> {
    let (remaining_input, (_, _, table, opt_materialized)) = tuple((
        multispace1,
        tag_no_case("TABLE"),
        table,
        opt(
            tuple((
                multispace1,
                alt((
                    tag_no_case("PRESERVE"),
                    tag_no_case("PURGE"),
                )),
                multispace1,
                tag_no_case("MATERIALIZED"),
                multispace1,
                tag_no_case("VIEW"),
                multispace1,
                tag_no_case("LOG"),
            )),
        ),
    ))(i)?;

    let materialized = opt_materialized
        .map(|(_, mat_option, _, _, _, _, _, _)| {
            let mat: MaterializedViewLogOption = bytes_to_string(mat_option).as_str().into();
            mat
        });

    Ok((remaining_input, TruncateType::Table(TruncateTable {
        table,
        materialized,
    })))
}


fn truncate_cluster_type(i: &[u8]) -> IResult<&[u8], TruncateType> {
    let (remaining_input, (_, _, cluster_name)) = tuple((
        multispace1,
        tag_no_case("CLUSTER"),
        table,
    ))(i)?;
    Ok((remaining_input,
        TruncateType::Cluster(TruncateCluster { cluster_name })
    ))
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Truncate {
    truncate_type: TruncateType,
    storage: Option<StorageOption>,
}

impl fmt::Display for Truncate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let storage = self.storage.as_ref()
            .map(|so| {
                match so {
                    StorageOption::Drop => "DROP",
                    StorageOption::Reuse => "REUSE",
                }
            })
            .map(|s| format!(" {} STORAGE", s))
            .unwrap_or("".to_string());
        write!(f, "TRUNCATE {}{};", self.truncate_type, storage)?;
        Ok(())
    }
}

pub fn truncate(i: &[u8]) -> IResult<&[u8], Truncate> {
    let (remaining_input, (_, _, truncate_type, opt, _, _, )) = tuple((
        multispace0,
        tag_no_case("TRUNCATE"),
        alt((
            truncate_table_type,
            truncate_cluster_type
        )),
        opt(tuple((
            multispace1,
            alt((
                tag_no_case("DROP"),
                tag_no_case("REUSE"),
            )
            ),
            multispace1,
            tag_no_case("STORAGE"),
        ))),
        multispace0,
        tag(";")
    ))(i)?;
    let storage = opt.map(|(_, b, _, _, )| {
        let string = bytes_to_string(b);
        let s = string.as_str();
        let storage: StorageOption = s.into();
        storage
    });
    Ok((remaining_input, Truncate { truncate_type, storage }))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_truncate() {
        assert_eq!(truncate(b"TRUNCATE Table matt;").unwrap().1,
                   Truncate {
                       truncate_type: TruncateType::Table(TruncateTable {
                           table: Table::new("matt".to_string()),
                           materialized: None,
                       }),
                       storage: None,
                   });

        assert_eq!(truncate(b"TRUNCATE Table matt purge MATERIALIZED VIEW LOG;").unwrap().1,
                   Truncate {
                       truncate_type: TruncateType::Table(TruncateTable {
                           table: Table::new("matt".to_string()),
                           materialized: Some(MaterializedViewLogOption::Purge),
                       }),
                       storage: None,
                   });
        assert_eq!(truncate(b"TRUNCATE Table matt Preserve MATERIALIZED VIEW LOG;").unwrap().1,
                   Truncate {
                       truncate_type: TruncateType::Table(TruncateTable {
                           table: Table::new("matt".to_string()),
                           materialized: Some(MaterializedViewLogOption::Preserve),
                       }),
                       storage: None,
                   });
        assert_eq!(truncate(b"TRUNCATE Cluster matt  ;").unwrap().1,
                   Truncate {
                       truncate_type: TruncateType::Cluster(TruncateCluster {
                           cluster_name: Table::new("matt".to_string()),
                       }),
                       storage: None,
                   });
        assert_eq!(truncate(b"TRUNCATE Cluster matt  Reuse STORAGE;").unwrap().1,
                   Truncate {
                       truncate_type: TruncateType::Cluster(TruncateCluster {
                           cluster_name: Table::new("matt".to_string()),
                       }),
                       storage: Some(StorageOption::Reuse),
                   });

        assert_eq!(truncate(b"TRUNCATE Table matt Preserve MATERIALIZED VIEW LOG Drop STORAGE;").unwrap().1,
                   Truncate {
                       truncate_type: TruncateType::Table(TruncateTable {
                           table: Table::new("matt".to_string()),
                           materialized: Some(MaterializedViewLogOption::Preserve),
                       }),
                       storage: Some(StorageOption::Drop),
                   });
    }

    #[test]
    fn fmt_truncate_table() {
        assert_eq!(format!("{}", TruncateTable {
            table: Table::new("maTable".to_string()),
            materialized: None,
        }), " TABLE maTable");

        assert_eq!(format!("{}", TruncateTable {
            table: Table::new("maTable".to_string()),
            materialized: Some(MaterializedViewLogOption::Preserve),
        }), " TABLE maTable PRESERVE MATERIALIZED VIEW LOG");

        assert_eq!(format!("{}", TruncateTable {
            table: Table::new_with_schema("sch".to_string(), "maTable".to_string()),
            materialized: Some(MaterializedViewLogOption::Purge),
        }), " TABLE sch.maTable PURGE MATERIALIZED VIEW LOG");
    }
}
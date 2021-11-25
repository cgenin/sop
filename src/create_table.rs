use std::fmt;

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::opt;
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::tuple;

use crate::data_types::{column_definition, ColumnDefinition};
use crate::table::{bytes_to_string, Table, table};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum CommitRows {
    None,
    Delete,
    Preserve,
}

impl fmt::Display for CommitRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CommitRows::Preserve => write!(f, " ON COMMIT PRESERVE ROWS")?,
            CommitRows::Delete => write!(f, " ON COMMIT DELETE ROWS")?,
            CommitRows::None => write!(f, "")?,
        }
        Ok(())
    }
}

pub type RelationProperties = Vec<ColumnDefinition>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct CreateTable {
    pub global_temporary: bool,
    pub table: Table,
    pub relational_properties: RelationProperties,
    pub commit_rows: CommitRows,
    // TODO
    physical_properties: bool,
    // TODO
    table_properties: bool,
}

fn relation_properties(i: &[u8]) -> IResult<&[u8], RelationProperties> {
    let (remaining_input, (_, _, _, v, _, _)) = tuple((
        multispace0,
        tag("("),
        multispace0,
        separated_list1(
            tuple((multispace0, tag(","), multispace0)),
            column_definition,
        ),
        multispace0,
        tag(")"),
    ))(i)?;
    Ok((remaining_input, v))
}

fn commit_rows(i: &[u8]) -> IResult<&[u8], CommitRows> {
    let result1: IResult<&[u8], (&[u8], &[u8])> = tuple((
        multispace1,
        alt((
            tag_no_case("ON COMMIT PRESERVE ROWS"),
            tag_no_case("ON COMMIT DELETE ROWS"),
        ))
    ))(i);
    let result = result1
        .map(|(remaining_input, t)| {
            let string = bytes_to_string(t.1);
            match string.as_str() {
                "ON COMMIT PRESERVE ROWS" => (remaining_input, CommitRows::Preserve),
                "ON COMMIT DELETE ROWS" => (remaining_input, CommitRows::Delete),
                _ => (remaining_input, CommitRows::None)
            }
        })
        .unwrap_or((i, CommitRows::None));
    Ok(result)
}


pub fn create_table(i: &[u8]) -> IResult<&[u8], CreateTable> {
    let (remaining_input, (_, _,
        opt_gt,
        _,
        table,
        opt_rp,
        _)) = tuple((
        tag_no_case("CREATE"),
        multispace1,
        opt(tuple((
            tag_no_case("GLOBAL"),
            multispace1,
            tag_no_case("temporary"),
            multispace1,
        ))
        ),
        tag_no_case("TABLE"),
        table,
        opt(relation_properties),
        tag(";")
    ))(i)?;
    let global_temporary = opt_gt.is_some();
    let relational_properties = opt_rp.unwrap_or(vec![]);
    let create_table = CreateTable {
        table,
        global_temporary,
        relational_properties,
        commit_rows: CommitRows::None,
        physical_properties: false,
        table_properties: false,
    };
    Ok((remaining_input, create_table))
}

#[cfg(test)]
mod tests {
    use crate::constraints::{Constraint, InlineOrOutlineConstraints};
    use crate::data_types::{DataType, NumberPrecision};

    use super::*;

    #[test]
    fn test_create_table() {
        assert_eq!(create_table(b"CREATE table MA_table;").unwrap().1, CreateTable {
            table: Table::new("MA_table"),
            global_temporary: false,
            relational_properties: Vec::new(),
            commit_rows: CommitRows::None,
            physical_properties: false,
            table_properties: false,
        });

        assert_eq!(create_table(b"CREATE table MA_table  (id NUMBER(12,2));").unwrap().1, CreateTable {
            table: Table::new("MA_table"),
            global_temporary: false,
            relational_properties: vec![
                ColumnDefinition::new("id", DataType::Number(NumberPrecision::new_with_scale(12,2)))
            ],
            commit_rows: CommitRows::None,
            physical_properties: false,
            table_properties: false,
        });

    }

    #[test]
    fn test_relation_properties() {
        assert_eq!(relation_properties(b" (TEST1 NUMBER(2))").unwrap().1, vec![
            ColumnDefinition::new("TEST1", DataType::Number(NumberPrecision::new(2)))
        ]);
        assert_eq!(relation_properties(b"(id NUMBER(12,2))").unwrap().1, vec![
            ColumnDefinition::new("id", DataType::Number(NumberPrecision::new_with_scale(12,2)))
        ]);
        assert_eq!(relation_properties(b" (TEST1 NUMBER(2), ID VARCHAR(32))").unwrap().1, vec![
            ColumnDefinition::new("TEST1", DataType::Number(NumberPrecision::new(2))),
            ColumnDefinition::new("ID", DataType::Varchar2(32)),

        ]);
        assert_eq!(relation_properties(b" (TEST1 NUMBER(2), ID VARCHAR(32) PRIMARY KEY)").unwrap().1, vec![
            ColumnDefinition::new("TEST1", DataType::Number(NumberPrecision::new(2))),
            ColumnDefinition::new("ID", DataType::Varchar2(32))
                .with_inline_constraints(InlineOrOutlineConstraints::new(Constraint::PrimaryKey(vec![]))),
        ]);
        assert_eq!(relation_properties(b" ( TEST1 NUMBER(2) )").unwrap().1, vec![
            ColumnDefinition::new("TEST1", DataType::Number(NumberPrecision::new(2)))
        ]);
    }

    #[test]
    fn test_commit_rows() {
        assert_eq!(commit_rows(b" ON COMMIT PRESERVE ROWS").unwrap().1, CommitRows::Preserve);
        assert_eq!(commit_rows(b" ON COMMIT DELETE ROWS").unwrap().1, CommitRows::Delete);
        assert_eq!(commit_rows(b"").unwrap().1, CommitRows::None);
        assert_eq!(commit_rows(b"test").unwrap().1, CommitRows::None);
    }
}
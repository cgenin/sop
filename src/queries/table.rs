use std::fmt;
use std::str;

use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::opt;
use nom::IResult;
use nom::sequence::tuple;

use crate::queries::commons::{bytes_to_string, sql_identifier};

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub alias: Option<String>,
    pub schema: Option<String>,
}

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref schema) = self.schema {
            write!(f, "{}.", schema)?;
        }
        write!(f, "{}", &self.name)?;
        if let Some(ref alias) = self.alias {
            write!(f, " AS {}", alias)?;
        }
        Ok(())
    }
}

impl<'a> From<&'a str> for Table {
    fn from(t: &str) -> Table {
        Table {
            name: String::from(t),
            alias: None,
            schema: None,
        }
    }
}

impl<'a> From<(&'a str, &'a str)> for Table {
    fn from(t: (&str, &str)) -> Table {
        Table {
            name: String::from(t.1),
            alias: None,
            schema: Some(String::from(t.0)),
        }
    }
}

impl Table {
    pub fn new(name_table: &str) -> Table {
        let name = name_table.to_string();
        Table {
            name,
            alias: None,
            schema: None,
        }
    }
    pub fn new_with_schema(schema_name: &str, name_table: &str) -> Table {
        let name = name_table.to_string();
        let schema = schema_name.to_string();
        Table {
            name,
            alias: None,
            schema: Some(schema),
        }
    }
}

pub fn schema(i: &[u8]) -> IResult<&[u8], String> {
    let (remaining_input, (sch, _, _, _)) = tuple((
        sql_identifier,
        multispace0,
        tag("."),
        multispace0
    ))(i)?;
    let schema = bytes_to_string(sch);
    Ok((remaining_input, schema))
}

pub fn alias(i: &[u8]) -> IResult<&[u8], String> {
    let (remaining_input, (_, _, _, al)) = tuple((
        multispace1,
        tag_no_case("AS"),
        multispace1,
        sql_identifier
    ))(i)?;
    Ok((remaining_input, bytes_to_string(al)))
}

pub fn table(i: &[u8]) -> IResult<&[u8], Table> {
    let (remaining_input, (_, schema, table_name, alias, )) = tuple((
        multispace1,
        opt(schema),
        sql_identifier,
        opt(alias),
    ))(i)?;

    let name = bytes_to_string(table_name);
    Ok((
        remaining_input,
        Table {
            name,
            schema,
            alias,
        }
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_table() {
        assert_eq!(table(b" maTable ").unwrap().1, Table {
            name: "maTable".to_string(),
            schema: None,
            alias: None,
        });
        assert_eq!(table(b" ma_Table ").unwrap().1, Table {
            name: "ma_Table".to_string(),
            schema: None,
            alias: None,
        });
        assert_eq!(table(b" ssds sss").unwrap().1, Table {
            name: "ssds".to_string(),
            schema: None,
            alias: None,
        });

        assert_eq!(table(b" monschema.maTable ").unwrap().1, Table {
            name: "maTable".to_string(),
            schema: Some("monschema".to_string()),
            alias: None,
        });

        assert_eq!(table(b" monschema\n.maTable ").unwrap().1, Table {
            name: "maTable".to_string(),
            schema: Some("monschema".to_string()),
            alias: None,
        });

        assert_eq!(table(b" maTable as il ").unwrap().1, Table {
            name: "maTable".to_string(),
            schema: None,
            alias: Some("il".to_string()),
        });

        assert_eq!(table(b" monschema\n.maTable As il ").unwrap().1, Table {
            name: "maTable".to_string(),
            schema: Some("monschema".to_string()),
            alias: Some("il".to_string()),
        });
    }
}
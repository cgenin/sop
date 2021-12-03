use std::fmt;
use std::str;

use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::opt;
use nom::IResult;
use nom::sequence::tuple;

use crate::queries::table::{Table, table};

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct DropTable {
    pub table: Table,
    pub cascade_constraints: bool,
    pub purge: bool,
}

impl fmt::Display for DropTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DROP TABLE {}", self.table)?;

        if self.cascade_constraints {
            write!(f, " {} {}", CASCADE, CONSTRAINTS)?;
        }
        if self.purge {
            write!(f, " PURGE")?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl DropTable {
    pub fn new(table: &str) -> DropTable {
        DropTable {
            table: Table::new(table),
            cascade_constraints: false,
            purge: false,
        }
    }
}


const CASCADE: &'static str = "CASCADE";

const CONSTRAINTS: &'static str = "CONSTRAINTS";

pub fn cascade_constraints(i: &[u8]) -> IResult<&[u8], bool> {
    let (remaining_input, o) = opt(tuple((
        multispace1,
        tag_no_case(CASCADE),
        multispace1,
        tag_no_case(CONSTRAINTS),
    )))(i)?;
    Ok((remaining_input, o.is_some()))
}

pub fn purge(i: &[u8]) -> IResult<&[u8], bool> {
    let (remaining_input, o) = opt(tuple((
        multispace1,
        tag_no_case("PURGE"),
    )))(i)?;
    Ok((remaining_input, o.is_some()))
}

pub fn drop_table(i: &[u8]) -> IResult<&[u8], DropTable> {
    let (remaining_input, (_, _, _, _, table, cascade_constraints, purge, _, _)) = tuple((
        multispace0,
        tag_no_case("DROP"),
        multispace1,
        tag_no_case("TABLE"),
        table,
        cascade_constraints,
        purge,
        multispace0,
        tag(";")
    ))(i)?;
    Ok((remaining_input, DropTable {
        table,
        cascade_constraints,
        purge,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_drop_table() {
        assert_eq!(drop_table(b"DROP table maTable;").unwrap().1, DropTable {
            table: Table {
                name: "maTable".to_string(),
                schema: None,
                alias: None,
            },
            cascade_constraints: false,
            purge: false,
        });

        assert_eq!(drop_table(b"DROP table maTable\n;").unwrap().1, DropTable {
            table: Table {
                name: "maTable".to_string(),
                schema: None,
                alias: None,
            },
            cascade_constraints: false,
            purge: false,
        });

        assert_eq!(drop_table(b"DROP table sch.maTable\n;").unwrap().1, DropTable {
            table: Table {
                name: "maTable".to_string(),
                schema: Some("sch".to_string()),
                alias: None,
            },
            cascade_constraints: false,
            purge: false,
        });
    }

    #[test]
    fn test_cascade_constraints() {
        assert_eq!(drop_table(b"DROP table maTable cascade CONSTRAINTS ;").unwrap().1, DropTable {
            table: Table {
                name: "maTable".to_string(),
                schema: None,
                alias: None,
            },
            cascade_constraints: true,
            purge: false,
        });

        assert_eq!(drop_table(b"DROP table maTable cascade\tCONSTRAINTS   ;").unwrap().1, DropTable {
            table: Table {
                name: "maTable".to_string(),
                schema: None,
                alias: None,
            },
            cascade_constraints: true,
            purge: false,
        });
    }

    #[test]
    fn test_purge() {
        assert!(purge(b" PURGE").unwrap().1);
        assert_eq!(purge(b" PURGA").unwrap().1, false);
        assert_eq!(purge(b"PURGE").unwrap().1, false);

        assert_eq!(drop_table(b"DROP table maTable purge;").unwrap().1, DropTable {
            table: Table {
                name: "maTable".to_string(),
                schema: None,
                alias: None,
            },
            cascade_constraints: false,
            purge: true,
        });
    }
}
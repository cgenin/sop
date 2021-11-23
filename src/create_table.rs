use std::fmt;

use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::opt;
use nom::IResult;
use nom::sequence::tuple;

use crate::data_types::ColumnDefinition;
use crate::table::{bytes_to_string, Table};

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
    global_temporary: bool,
    table: Table,
    relational_properties: RelationProperties,
    commit_rows: CommitRows,
    // TODO
    physical_properties: bool,
    // TODO
    table_properties: bool,
}

fn commit_rows(i: &[u8]) -> IResult<&[u8], CommitRows> {
    let result1:IResult<&[u8],(&[u8], &[u8])> = tuple((
        multispace1,
        alt((
            tag_no_case("ON COMMIT PRESERVE ROWS"),
            tag_no_case("ON COMMIT DELETE ROWS"),
        ))
    ))(i);
    let result= result1
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
use std::fmt;
use log::debug;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, multispace0, multispace1};
use nom::IResult;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::tuple;

use crate::queries::commons::{bytes_to_string, Comment, comment};
use crate::queries::create_table::{create_table, CreateTable};
use crate::queries::drop_table::{drop_table, DropTable};
use crate::queries::truncate::{Truncate, truncate};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Sql {
    Empty,
    Comment(Comment),
    CreateTable(CreateTable),
    DropTable(DropTable),
    Truncate(Truncate),

}

impl fmt::Display for Sql {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Sql::Comment(c) => write!(f, "{}", c)?,
            Sql::CreateTable(c) => write!(f, "{}", c)?,
            Sql::DropTable(c) => write!(f, "{}", c)?,
            Sql::Truncate(c) => write!(f, "{}", c)?,
            Sql::Empty => write!(f, "\n")?,
        }
        Ok(())
    }
}

fn parser_empty(i: &[u8]) -> IResult<&[u8], Sql> {
    let (remaining_input, _) = multispace1(i)?;
    Ok((remaining_input, Sql::Empty))
}

fn parser_comment(i: &[u8]) -> IResult<&[u8], Sql> {
    let (remaining_input, c) = comment(i)?;
    Ok((remaining_input, Sql::Comment(c)))
}

fn parser_createtable(i: &[u8]) -> IResult<&[u8], Sql> {
    let (remaining_input, c) = create_table(i)?;
    Ok((remaining_input, Sql::CreateTable(c)))
}

fn parser_dropttable(i: &[u8]) -> IResult<&[u8], Sql> {
    let (remaining_input, c) = drop_table(i)?;
    Ok((remaining_input, Sql::DropTable(c)))
}

fn parser_truncate(i: &[u8]) -> IResult<&[u8], Sql> {
    let (remaining_input, c) = truncate(i)?;
    Ok((remaining_input, Sql::Truncate(c)))
}

pub fn parse(i: &[u8]) -> IResult<&[u8], Vec<Sql>> {
    let (remaining_input, values) = separated_list0(
        multispace1,
        alt((
            parser_dropttable,
            parser_createtable,
            parser_truncate,
            parser_comment,
            parser_empty,

        )))
        (i)?;
    let string = bytes_to_string(remaining_input.clone());
    debug!("****{}****", string);
    Ok((remaining_input, values))
}

pub fn parse_str(s: &str) -> IResult<&[u8], Vec<Sql>> {
    parse(s.as_bytes())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        assert_eq!(parse(b"DROP TABLE test;
            TRUNCATE Table matt;").unwrap().1,
                   vec![Sql::DropTable(DropTable::new("test")),
                   Sql::Truncate(Truncate::new_table("matt"))]);
        assert_eq!(parse(b"-- text
            TRUNCATE Table matt;").unwrap().1,
                   vec![Sql::Comment(Comment::new(" text")),
                        Sql::Truncate(Truncate::new_table("matt"))]);
    }
}
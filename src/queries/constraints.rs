use std::fmt;

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::opt;
use nom::error::{Error, ErrorKind};
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::tuple;

use crate::queries::commons::{sql_identifier, bytes_to_string};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ConstraintsState {
    NotDeferrable,
    Deferrable,
    InitiallyImmediate,
    InitiallyDeferred,
    ENABLE,
    DISABLE,
    VALIDATE,
    NoValidate,
    Rely,
    NoRely,
    List(Box<Vec<ConstraintsState>>),
}

impl fmt::Display for ConstraintsState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstraintsState::NotDeferrable => write!(f, "NOT DEFERRABLE")?,
            ConstraintsState::Deferrable => write!(f, "DEFERRABLE")?,
            ConstraintsState::InitiallyImmediate => write!(f, "INITIALLY IMMEDIATE")?,
            ConstraintsState::InitiallyDeferred => write!(f, "INITIALLY DEFERRED")?,
            ConstraintsState::ENABLE => write!(f, "ENABLE")?,
            ConstraintsState::DISABLE => write!(f, "DISABLE")?,
            ConstraintsState::VALIDATE => write!(f, "VALIDATE")?,
            ConstraintsState::NoValidate => write!(f, "NOVALIDATE")?,
            ConstraintsState::Rely => write!(f, "RELY")?,
            ConstraintsState::NoRely => write!(f, "NORELY")?,
            ConstraintsState::List(v) => {
                let strings: Vec<String> = v.iter()
                    .map(|e| format!("{}", e))
                    .collect();
                let parameters = strings.join(" ");
                write!(f, "{}", parameters)?;
            }
        }
        Ok(())
    }
}

fn terminal_constraints_state(i: &[u8]) -> IResult<&[u8], ConstraintsState> {
    let (remaining_input, s) = alt((
        tag_no_case("NOT DEFERRABLE"),
        tag_no_case("DEFERRABLE"),
        tag_no_case("INITIALLY IMMEDIATE"),
        tag_no_case("INITIALLY DEFERRED"),
        tag_no_case("ENABLE"),
        tag_no_case("DISABLE"),
        tag_no_case("VALIDATE"),
        tag_no_case("NOVALIDATE"),
        tag_no_case("RELY"),
        tag_no_case("NORELY"),
    ))(i)?;
    let value = bytes_to_string(s).to_uppercase();
    match value.as_str() {
        "NOT DEFERRABLE" => Ok((remaining_input, ConstraintsState::NotDeferrable)),
        "DEFERRABLE" => Ok((remaining_input, ConstraintsState::Deferrable)),
        "INITIALLY IMMEDIATE" => Ok((remaining_input, ConstraintsState::InitiallyImmediate)),
        "INITIALLY DEFERRED" => Ok((remaining_input, ConstraintsState::InitiallyDeferred)),
        "ENABLE" => Ok((remaining_input, ConstraintsState::ENABLE)),
        "DISABLE" => Ok((remaining_input, ConstraintsState::DISABLE)),
        "VALIDATE" => Ok((remaining_input, ConstraintsState::VALIDATE)),
        "NOVALIDATE" => Ok((remaining_input, ConstraintsState::NoValidate)),
        "RELY" => Ok((remaining_input, ConstraintsState::Rely)),
        "NORELY" => Ok((remaining_input, ConstraintsState::NoRely)),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Digit)))
    }
}

fn list_constraints_state(i: &[u8]) -> IResult<&[u8], ConstraintsState> {
    let (remaining_input, vec) = separated_list1(tag(" "), terminal_constraints_state)(i)?;
    if vec.len() == 1 {
        return Ok((remaining_input, vec.get(0).unwrap().clone()));
    }
    let v = Box::new(vec);
    Ok((remaining_input, ConstraintsState::List(v.into())))
}

pub fn constraints_state(i: &[u8]) -> IResult<&[u8], ConstraintsState> {
    let (remaining_input, result) = alt((
        list_constraints_state,
        terminal_constraints_state,
    ))(i)?;
    Ok((remaining_input, result))
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Constraint {
    Unique(Vec<String>),
    PrimaryKey(Vec<String>),
    ForeignKey(Vec<String>),
}


impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constraint::PrimaryKey(v) => {
                write!(f, "PRIMARY KEY")?;
                if v.len() > 0 {
                    write!(f, " ({})", v.join(", "))?;
                }
            }
            Constraint::Unique(v) => {
                write!(f, "UNIQUE")?;
                if v.len() > 0 {
                    write!(f, " ({})", v.join(", "))?;
                }
            }
            Constraint::ForeignKey(v) => {
                write!(f, "FOREIGN KEY")?;
                if v.len() > 0 {
                    write!(f, " ({})", v.join(", "))?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct InlineOrOutlineConstraints {
    constraint_name: Option<String>,
    constraint: Constraint,
    constraints_state: Option<ConstraintsState>,
}


impl fmt::Display for InlineOrOutlineConstraints {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.constraint_name.is_some() {
            write!(f, "CONSTRAINT {} ", self.constraint_name.as_ref().unwrap())?;
        }
        write!(f, "{}", self.constraint)?;
        if self.constraints_state.is_some() {
            write!(f, " {}", self.constraints_state.as_ref().unwrap())?;
        }
        Ok(())
    }
}

impl InlineOrOutlineConstraints {
    pub fn new(constraint: Constraint) -> InlineOrOutlineConstraints {
        InlineOrOutlineConstraints { constraint, constraints_state: None, constraint_name: None }
    }

    pub fn new_with_name(constraint_name: String, constraint: Constraint) -> InlineOrOutlineConstraints {
        InlineOrOutlineConstraints { constraint, constraints_state: None, constraint_name: Some(constraint_name) }
    }

    pub fn new_with_constraints_state(constraint: Constraint, constraints_state: ConstraintsState) -> InlineOrOutlineConstraints {
        InlineOrOutlineConstraints { constraint, constraints_state: Some(constraints_state), constraint_name: None }
    }

    pub fn new_with_all(constraint_name: String, constraint: Constraint, constraints_state: ConstraintsState) -> InlineOrOutlineConstraints {
        InlineOrOutlineConstraints { constraint, constraints_state: Some(constraints_state), constraint_name: Some(constraint_name) }
    }
}

fn constraint(i: &[u8]) -> IResult<&[u8], Constraint> {
    let (remaining_input, (typ, opt)) = tuple((
        alt((
            tag_no_case("UNIQUE"),
            tag_no_case("PRIMARY KEY"),
            tag_no_case("FOREIGN KEY"),
        )),
        opt(tuple((
            multispace0,
            tag("("),
            multispace0,
            separated_list1(
                tuple((
                    multispace0,
                    tag(","),
                    multispace0,
                )),
                sql_identifier,
            ),
            multispace0,
            tag(")")
        )))
    ))(i)?;
    let resuts: Vec<String> = opt.map(|(_, _, _, columns, _, _, )| columns.iter()
        .map(|b| bytes_to_string(b))
        .collect()
    )
        .unwrap_or(Vec::new());
    let t = bytes_to_string(typ).to_uppercase();
    match t.as_str() {
        "PRIMARY KEY" => Ok((remaining_input, Constraint::PrimaryKey(resuts))),
        "UNIQUE" => Ok((remaining_input, Constraint::Unique(resuts))),
        "FOREIGN KEY" => Ok((remaining_input, Constraint::ForeignKey(resuts))),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Tag)))
    }
}

pub fn inline_or_outline_constraint(i: &[u8]) -> IResult<&[u8], InlineOrOutlineConstraints> {
    let (remaining_input, (cn, constraint, cs)) = tuple((
        opt(tuple((
            tag_no_case("CONSTRAINT"),
            multispace1,
            sql_identifier,
            multispace1,
        ))
        ),
        constraint,
        opt(tuple((
            multispace1,
            constraints_state,
        ))
        ),
    ))(i)?;
    let inline_or_outline_constraints = cn.as_ref().map(|(_, _, s, _)| {
        let constraint_name = bytes_to_string(s);
        cs.as_ref().map(|(_, c)| {
            InlineOrOutlineConstraints::new_with_all(constraint_name.clone(), constraint.clone(), c.clone())
        })
            .unwrap_or(InlineOrOutlineConstraints::new_with_name(constraint_name.clone(), constraint.clone()))
    })
        .unwrap_or_else(|| {
            cs.as_ref().map(|(_, c)| {
                InlineOrOutlineConstraints::new_with_constraints_state(constraint.clone(), c.clone())
            })
                .unwrap_or(InlineOrOutlineConstraints::new(constraint.clone()))
        });
    Ok((remaining_input, inline_or_outline_constraints))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constraints_state() {
        assert_eq!(constraints_state(b"NOT DEFERRABLE RELY VALIDATE").unwrap().1,
                   ConstraintsState::List(Box::new(vec![
                       ConstraintsState::NotDeferrable, ConstraintsState::Rely, ConstraintsState::VALIDATE,
                   ]))
        );

        assert_eq!(constraints_state(b"NOVALIDATE").unwrap().1,
                   ConstraintsState::NoValidate
        );
        assert_eq!(constraints_state(b"INITIALLY IMMEDIATE").unwrap().1,
                   ConstraintsState::InitiallyImmediate
        );
    }
}
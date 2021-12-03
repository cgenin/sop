use std::fmt;

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{digit1, multispace0, multispace1};
use nom::combinator::opt;
use nom::error::{Error, ErrorKind};
use nom::IResult;
use nom::multi::separated_list0;
use nom::sequence::tuple;

use crate::queries::commons::{Expression, expression, len_as_u16, sql_identifier, bytes_to_string};
use crate::queries::constraints::{inline_or_outline_constraint, InlineOrOutlineConstraints};
use crate::queries::data_types::DataType::Number;
use crate::queries::encryption::{encryption_spec, EncryptionSpec};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct NumberPrecision {
    pub precision: Option<u16>,
    pub scale: Option<u16>,
}


impl fmt::Display for NumberPrecision {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.precision.is_some() {
            let precision = self.precision.unwrap();
            let value = self.scale.map(|s| format!("{},{}", precision, s))
                .unwrap_or(format!("{}", precision));
            write!(f, "[{}]", value)?;
        }
        Ok(())
    }
}

impl NumberPrecision {
    pub fn empty() -> NumberPrecision {
        NumberPrecision {
            precision: None,
            scale: None,
        }
    }
    pub fn new(precision: u16) -> NumberPrecision {
        NumberPrecision {
            precision: Some(precision),
            scale: None,
        }
    }
    pub fn new_with_scale(precision: u16, scale: u16) -> NumberPrecision {
        NumberPrecision {
            precision: Some(precision),
            scale: Some(scale),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum TimeZone {
    None,
    WithTimeZone,
    WithLocalTimeZone,
}

impl fmt::Display for TimeZone {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TimeZone::WithTimeZone => write!(f, "{}", " WITH TIME ZONE"),
            TimeZone::WithLocalTimeZone => write!(f, "{}", " WITH LOCAL TIME ZONE"),
            TimeZone::None => Ok(())
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum DataType {
    // Character Datatypes
    Char(u16),
    Varchar2(u16),
    NChar(u16),
    NVarchar2(u16),
    Number(NumberPrecision),
    Timestamp(TimeZone),
    Date,
    Blob,
    Clob,
    NClob,
    BFile,
    XMLType,
    UriType,
    Raw,

}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DataType::Char(p) => write!(f, "{}", p)?,
            DataType::Varchar2(p) => write!(f, "{}", p)?,
            DataType::NChar(p) => write!(f, "{}", p)?,
            DataType::NVarchar2(p) => write!(f, "{}", p)?,
            DataType::Number(p) => write!(f, "{}", p)?,
            DataType::Timestamp(p) => write!(f, "{}", p)?,
            DataType::Date => write!(f, "Date")?,
            DataType::Blob => write!(f, "Blob")?,
            DataType::Clob => write!(f, "Clob")?,
            DataType::NClob => write!(f, "NClob")?,
            DataType::BFile => write!(f, "BFile")?,
            DataType::XMLType => write!(f, "XMLType")?,
            DataType::UriType => write!(f, "UriType")?,
            DataType::Raw => write!(f, "Raw")?,
        }
        Ok(())
    }
}

fn character_types(i: &[u8]) -> IResult<&[u8], DataType> {
    let (remaining_input, (type_in_bytes, _, length, _, _, _)) = tuple((
        alt((
            tag_no_case("Char"),
            tag_no_case("Varchar2"),
            tag_no_case("Varchar"),
            tag_no_case("NChar"),
            tag_no_case("NVarchar2"),
        )),
        tag("("),
        digit1,
        opt(tuple((
            multispace1,
            tag_no_case("BYTE")
        ))),
        multispace0,
        tag(")"),
    ))(i)?;
    let string = bytes_to_string(type_in_bytes).to_uppercase();
    let type_of_chars = string.as_str();
    match type_of_chars {
        "CHAR" => Ok((remaining_input, DataType::Char(len_as_u16(length)))),
        "VARCHAR2" => Ok((remaining_input, DataType::Varchar2(len_as_u16(length)))),
        "VARCHAR" => Ok((remaining_input, DataType::Varchar2(len_as_u16(length)))),
        "NCHAR" => Ok((remaining_input, DataType::NChar(len_as_u16(length)))),
        "NVARCHAR2" => Ok((remaining_input, DataType::NVarchar2(len_as_u16(length)))),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Tag)))
    }
}

fn numeric_types(i: &[u8]) -> IResult<&[u8], DataType> {
    let (remaining_input, (_, opt_precision)) = tuple((
        tag_no_case("NUMBER"),
        opt(tuple((
            tag("("),
            multispace0,
            digit1,
            opt(tuple((
                multispace0,
                tag(","),
                multispace0,
                digit1,
            ))),
            multispace0,
            tag(")"),
        ))
        )
    ))(i)?;
    let number_precision = opt_precision
        .map(|(_, _, d, opt_scale, _, _, )| {
            let precision = len_as_u16(d);
            opt_scale.map(|(_, _, _, s)| {
                let scale = len_as_u16(s);
                NumberPrecision::new_with_scale(precision, scale)
            })
                .unwrap_or(NumberPrecision::new(precision))
        })
        .unwrap_or(NumberPrecision::empty());
    Ok((remaining_input, Number(number_precision)))
}

fn timestamp_types(i: &[u8]) -> IResult<&[u8], DataType> {
    let (remaining_input, (_, opt)) = tuple((
        tag_no_case("Timestamp"),
        opt(tuple((
            multispace1,
            tag_no_case("WITH"),
            opt(tuple((
                multispace1,
                tag_no_case("LOCAL"),
            ))),
            multispace1,
            tag_no_case("TIME"),
            multispace1,
            tag_no_case("ZONE"),
        ))),
    ))(i)?;
    let time_zone = opt.map(|(_, _, o, _, _, _, _, )| {
        o.map(|_| TimeZone::WithLocalTimeZone)
            .unwrap_or(TimeZone::WithTimeZone)
    })
        .unwrap_or(TimeZone::None);
    Ok((remaining_input, DataType::Timestamp(time_zone)))
}

fn basic_types(i: &[u8]) -> IResult<&[u8], DataType> {
    let (remaining_input, b) = alt((
        tag_no_case("Date"),
        tag_no_case("Blob"),
        tag_no_case("Clob"),
        tag_no_case("NClob"),
        tag_no_case("BFile"),
        tag_no_case("XMLType"),
        tag_no_case("UriType"),
        tag_no_case("Raw"),
    ))(i)?;

    let string = bytes_to_string(b).to_uppercase();
    match string.as_str() {
        "DATE" => Ok((remaining_input, DataType::Date)),
        "BLOB" => Ok((remaining_input, DataType::Blob)),
        "CLOB" => Ok((remaining_input, DataType::Clob)),
        "NCLOB" => Ok((remaining_input, DataType::NClob)),
        "BFILE" => Ok((remaining_input, DataType::BFile)),
        "XMLTYPE" => Ok((remaining_input, DataType::XMLType)),
        "URITYPE" => Ok((remaining_input, DataType::UriType)),
        "RAW" => Ok((remaining_input, DataType::Raw)),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Tag)))
    }
}

pub fn data_types(i: &[u8]) -> IResult<&[u8], DataType> {
    let (remaining_input, data_type) = alt((
        character_types,
        timestamp_types,
        numeric_types,
        basic_types,
    ))(i)?;
    Ok((remaining_input, data_type))
}


#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct ColumnDefinition {
    pub column_name: String,
    pub datatype: DataType,
    pub sort: bool,
    pub default: Option<Expression>,
    pub encrypt: Option<EncryptionSpec>,
    pub inline_constraints: Vec<InlineOrOutlineConstraints>,
}

impl fmt::Display for ColumnDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.column_name, self.datatype)?;
        if self.sort {
            write!(f, " SORT")?;
        }
        if self.default.is_some() {
            write!(f, " DEFAULT {}", self.default.as_ref().unwrap())?;
        }
        if self.encrypt.is_some() {
            write!(f, " ENCRYPT {}", self.encrypt.as_ref().unwrap())?;
        }
        let strings: Vec<String> = self.inline_constraints.iter().map(|i| format!("{}", i)).collect();
        write!(f, " {}", strings.join(" "))?;
        Ok(())
    }
}

impl ColumnDefinition {
    pub fn new(name: &str, datatype: DataType) -> ColumnDefinition {
        let column_name = name.to_string();
        ColumnDefinition {
            column_name,
            datatype,
            sort: false,
            default: None,
            encrypt: None,
            inline_constraints: Vec::new(),
        }
    }

    pub fn with_inline_constraints(&self, constraint: InlineOrOutlineConstraints) -> ColumnDefinition {
        let column_name = self.column_name.clone();
        let datatype = self.datatype.clone();
        let sort = self.sort;
        let default = self.default.clone();
        let encrypt = self.encrypt.clone();
        let mut inline_constraints = self.inline_constraints.clone();
        inline_constraints.push(constraint);
        ColumnDefinition {
            column_name,
            datatype,
            sort,
            default,
            encrypt,
            inline_constraints,
        }
    }
}

pub fn column_definition(i: &[u8]) -> IResult<&[u8], ColumnDefinition> {
    let (remaining_input, (cn, _, datatype, opt_sort, opt_defualt, opt_encrypt, _, inline_constraints)) = tuple((
        sql_identifier,
        multispace1,
        data_types,
        opt(
            tuple((
                multispace1,
                tag_no_case("SORT")
            ))
        ),
        opt(
            tuple((
                multispace1,
                tag_no_case("DEFAULT"),
                multispace1,
                expression
            ))
        ),
        opt(
            tuple((
                multispace1,
                tag_no_case("ENCRYPT"),
                multispace1,
                encryption_spec
            ))
        ),
        multispace0,
        separated_list0(tag(" "), inline_or_outline_constraint)
    ))(i)?;
    let column_name = bytes_to_string(cn);
    let sort = opt_sort.is_some();
    let default = opt_defualt.map(|t| t.3);
    let encrypt = opt_encrypt.map(|t| t.3);
    Ok((remaining_input, ColumnDefinition {
        column_name,
        datatype,
        sort,
        default,
        encrypt,
        inline_constraints,
    }))
}


#[cfg(test)]
mod tests {
    use crate::queries::commons::Prefix;

    use super::*;

    #[test]
    fn test_column_definition() {
        assert_eq!(column_definition(b"TEST1 NUMBER(2)").unwrap().1, ColumnDefinition {
            column_name: "TEST1".to_string(),
            datatype: DataType::Number(NumberPrecision::new(2)),
            sort: false,
            default: None,
            encrypt: None,
            inline_constraints: Vec::new(),
        });
        assert_eq!(column_definition(b"TEST1 NUMBER(2) SORT").unwrap().1, ColumnDefinition {
            column_name: "TEST1".to_string(),
            datatype: DataType::Number(NumberPrecision::new(2)),
            sort: true,
            default: None,
            encrypt: None,
            inline_constraints: Vec::new(),
        });
        assert_eq!(column_definition(b"TEST1 NUMBER(2) DEFAULT NOT NULL").unwrap().1, ColumnDefinition {
            column_name: "TEST1".to_string(),
            datatype: DataType::Number(NumberPrecision::new(2)),
            sort: false,
            default: Some(Expression::Prefix(Prefix::new("NOT", Expression::Null))),
            encrypt: None,
            inline_constraints: Vec::new(),
        });
    }

    #[test]
    fn test_character_types() {
        assert_eq!(character_types(b"VARCHAR(100)").unwrap().1, DataType::Varchar2(100));
        assert_eq!(character_types(b"VARCHAR2(100)").unwrap().1, DataType::Varchar2(100));
        assert_eq!(character_types(b"CHAR(50)").unwrap().1, DataType::Char(50));
    }

    #[test]
    fn test_numeric_types() {
        assert_eq!(numeric_types(b"Number").unwrap().1, DataType::Number(NumberPrecision { precision: None, scale: None }));
        assert_eq!(numeric_types(b"Number( 12)").unwrap().1, DataType::Number(NumberPrecision { precision: Some(12), scale: None }));
        assert_eq!(numeric_types(b"Number(12,  2 )").unwrap().1, DataType::Number(NumberPrecision { precision: Some(12), scale: Some(2) }));
    }

    #[test]
    fn test_timestamp_types() {
        assert_eq!(timestamp_types(b"Timestamp").unwrap().1, DataType::Timestamp(TimeZone::None));
        assert_eq!(timestamp_types(b"TIMESTAMP WITH TIME ZONE").unwrap().1, DataType::Timestamp(TimeZone::WithTimeZone));
        assert_eq!(timestamp_types(b"TIMESTAMP WITH  LOCAL TIME ZONE").unwrap().1, DataType::Timestamp(TimeZone::WithLocalTimeZone));
    }

    #[test]
    fn test_basic_types() {
        assert_eq!(basic_types(b"DATE").unwrap().1, DataType::Date);
        assert_eq!(basic_types(b"BLOB").unwrap().1, DataType::Blob);
        assert_eq!(basic_types(b"CLOB").unwrap().1, DataType::Clob);
        assert_eq!(basic_types(b"NCLOB").unwrap().1, DataType::NClob);
        assert_eq!(basic_types(b"BFILE").unwrap().1, DataType::BFile);
        assert_eq!(basic_types(b"XMLTYPE").unwrap().1, DataType::XMLType);
        assert_eq!(basic_types(b"URITYPE").unwrap().1, DataType::UriType);
        assert_eq!(basic_types(b"RAW").unwrap().1, DataType::Raw);
    }

    #[test]
    fn test_data_types() {
        assert_eq!(data_types(b"DATE").unwrap().1, DataType::Date);
        assert_eq!(data_types(b"NCHAR(50)").unwrap().1, DataType::NChar(50));
        assert_eq!(data_types(b"TIMESTAMP WITH TIME ZONE").unwrap().1, DataType::Timestamp(TimeZone::WithTimeZone));
        assert_eq!(data_types(b"Number(12)").unwrap().1, DataType::Number(NumberPrecision { precision: Some(12), scale: None }));
    }
}
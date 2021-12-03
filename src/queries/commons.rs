use std::fmt;
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while, take_while1};
use nom::character::{is_alphabetic, is_alphanumeric};
use nom::character::complete::{digit1, multispace0, multispace1};
use nom::combinator::opt;
use nom::error::{Error, ErrorKind};
use nom::IResult;
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, tuple};

#[inline]
pub fn is_sql_identifier(chr: u8) -> bool {
    is_alphanumeric(chr) || chr == '_' as u8
}

pub fn is_column_name(chr: u8) -> bool {
    is_alphabetic(chr) || chr == '_' as u8
}

#[inline]
pub fn len_as_u16(len: &[u8]) -> u16 {
    match std::str::from_utf8(len) {
        Ok(s) => match u16::from_str(s) {
            Ok(v) => v,
            Err(e) => panic!("{}", e),
        },
        Err(e) => panic!("{}", e),
    }
}

pub fn sql_identifier(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        delimited(tag("`"), take_while1(is_sql_identifier), tag("`")),
        delimited(tag("["), take_while1(is_sql_identifier), tag("]")),
        take_while1(is_sql_identifier),
    ))(i)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Parenthesis {
    exp: Box<Expression>,
}

impl fmt::Display for Parenthesis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.exp)?;
        Ok(())
    }
}

impl Parenthesis {
    pub fn new(exp: Expression) -> Parenthesis {
        let exp = Box::new(exp);
        Parenthesis { exp }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Prefix {
    prefix: String,
    exp: Box<Expression>,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.prefix, self.exp)?;
        Ok(())
    }
}

impl Prefix {
    pub fn new(prefixe: &str, expression: Expression) -> Prefix {
        let prefix = prefixe.to_string();
        let exp = Box::new(expression);
        Prefix {
            prefix,
            exp,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Operator {
    operator_name: String,
    exp_left: Box<Expression>,
    exp_right: Box<Expression>,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.exp_left, self.operator_name, self.exp_right)?;
        Ok(())
    }
}

impl Operator {
    pub fn new(operator_nam: &str, expression_left: Expression, expression_right: Expression) -> Operator {
        let operator_name = operator_nam.to_string();
        let exp_left = Box::new(expression_left.clone());
        let exp_right = Box::new(expression_right.clone());
        Operator { operator_name, exp_left, exp_right }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Function {
    function_name: String,
    parameters: Vec<Box<Expression>>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings: Vec<String> = self.parameters.iter()
            .map(|e| format!("{}", e))
            .collect();
        let parameters = strings.join(", ");
        write!(f, "{}({})", self.function_name, parameters)?;
        Ok(())
    }
}

impl Function {
    pub fn new(name: &str,
               params: Vec<Expression>) -> Function {
        let function_name = name.to_string();
        let parameters = params.iter().map(|e| Box::new(e.clone())).collect();
        Function { function_name, parameters }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct ComparisonOperator {
    preceded: String,
    operator_name: String,
    expression_left: Box<Expression>,
    expressions_right: Vec<Box<Expression>>,
}

impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings: Vec<String> = self.expressions_right.iter()
            .map(|e| format!("{}", e))
            .collect();
        let parameters = strings.join(", ");
        write!(f, "{} {} {} ({})", self.expression_left,
               self.preceded, self.operator_name, parameters)?;
        Ok(())
    }
}

impl ComparisonOperator {
    pub fn new(preceded_op: &str,
               name: &str,
               exp_left: Expression,
               params: Vec<Expression>) -> ComparisonOperator {
        let preceded = preceded_op.to_string();
        let operator_name = name.to_string();
        let expression_left = Box::new(exp_left);
        let expressions_right = params.iter().map(|e| Box::new(e.clone())).collect();
        ComparisonOperator { preceded, operator_name, expression_left, expressions_right }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Simple Expression
    Null,
    String(String),
    Number(u32, u32),
    SequenceCurval(String),
    SequenceNextval(String),
    Rownum,
    Column(ColumnExpression),
    // compound expression
    Parenthesis(Parenthesis),
    Prefix(Prefix),
    Operator(Operator),
    Function(Function),
    ComparisonOperator(ComparisonOperator),
    SubSelect, // TODO,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Null => write!(f, "NULL")?,
            Expression::String(s) => write!(f, "'{}'", s)?,
            Expression::Number(entier, decimal) => {
                if decimal > &(0 as u32) {
                    write!(f, "{}.{}", entier, decimal)?;
                } else {
                    write!(f, "{}", entier)?;
                }
            }
            Expression::SequenceCurval(sequence_name) => write!(f, "{}.CURVAL", sequence_name)?,
            Expression::SequenceNextval(sequence_name) => write!(f, "{}.NEXTVAL", sequence_name)?,
            Expression::Rownum => write!(f, "ROWNUM")?,
            Expression::Column(ce) => write!(f, "{}", ce)?,
            Expression::Parenthesis(exp) => write!(f, "{}", exp)?,
            Expression::Prefix(prefix) => write!(f, "{}", prefix)?,
            Expression::Operator(op) => write!(f, "{}", op)?,
            Expression::Function(func) => write!(f, "{}", func)?,
            Expression::ComparisonOperator(func) => write!(f, "{}", func)?,
            Expression::SubSelect => write!(f, "")?,
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct ColumnExpression {
    schema: Option<String>,
    alias_or_table: Option<String>,
    column_name: String,
}

impl ColumnExpression {
    pub fn new(name: &str) -> ColumnExpression {
        let column_name = name.to_string();
        ColumnExpression { column_name, alias_or_table: None, schema: None }
    }
    pub fn new_with_table(name: &str, table: &str) -> ColumnExpression {
        let column_name = name.to_string();
        ColumnExpression { column_name, alias_or_table: Some(table.to_string()), schema: None }
    }

    pub fn new_with_schema(name: &str, table: &str, schema: &str) -> ColumnExpression {
        let column_name = name.to_string();
        ColumnExpression { column_name, alias_or_table: Some(table.to_string()), schema: Some(schema.to_string()) }
    }
}

impl fmt::Display for ColumnExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.schema.is_some() {
            write!(f, "{}.", self.schema.as_ref().unwrap())?;
        }
        if self.alias_or_table.is_some() {
            write!(f, "{}.", self.alias_or_table.as_ref().unwrap())?;
        }
        write!(f, "{}", self.column_name)?;
        Ok(())
    }
}

fn null_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, _) = tag_no_case("NULL")(i)?;
    Ok((remaining_input, Expression::Null))
}

fn string_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, value) = delimited(
        tag("'"),
        take_while1(|s| s != b'\''),
        tag("'"))(i)?;
    let string = bytes_to_string(value);
    Ok((remaining_input, Expression::String(string)))
}

fn number_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (entier, opt_decimal)) =
        tuple((
            digit1,
            opt(tuple((tag("."), digit1)))
        ))
            (i)?;
    let entier = bytes_to_string(entier)
        .parse::<u32>()
        .map_err(|_| nom::Err::Error(Error::new(i, ErrorKind::Digit)))?;

    let decimal = opt_decimal
        .map(|(_, s)| bytes_to_string(s))
        .unwrap_or("0".to_string())
        .parse::<u32>()
        .map_err(|_| nom::Err::Error(Error::new(i, ErrorKind::Digit)))?;


    Ok((remaining_input, Expression::Number(entier, decimal)))
}

fn sequence_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (name, _, type_s)) = tuple((
        sql_identifier,
        tag("."),
        alt((
            tag_no_case("CURRVAL"),
            tag_no_case("NEXTVAL"),
        ))
    ))(i)?;
    let sequence_name = bytes_to_string(name);
    let string = bytes_to_string(type_s).to_uppercase();
    match string.as_str() {
        "CURRVAL" => Ok((remaining_input, Expression::SequenceCurval(sequence_name))),
        "NEXTVAL" => Ok((remaining_input, Expression::SequenceNextval(sequence_name))),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Tag)))
    }
}

fn rownum_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, _) = tag_no_case("ROWNUM")(i)?;
    Ok((remaining_input, Expression::Rownum))
}

fn column_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (opt_schema, opt_table, column)) = tuple((
        opt(tuple((sql_identifier, tag(".")))),
        opt(tuple((sql_identifier, tag(".")))),
        take_while1(is_alphabetic)
    ))(i)?;
    let column_name = bytes_to_string(column);
    if column_name.eq_ignore_ascii_case("NULL") || column_name.eq_ignore_ascii_case("ROWNUM") {
        return Err(nom::Err::Error(Error::new(i, ErrorKind::Digit)));
    }
    let column_expression = opt_schema.map(|(sch, _)| {
        let schema = bytes_to_string(sch);
        opt_table.map(|(ta, _)| {
            let table = bytes_to_string(ta);
            ColumnExpression::new_with_schema(column_name.as_str(), table.as_str(), schema.as_str())
        })
            .unwrap_or(ColumnExpression::new_with_table(column_name.as_str(), schema.as_str()))
    }).unwrap_or(ColumnExpression::new(column_name.as_str()));
    Ok((remaining_input, Expression::Column(column_expression)))
}

fn parenthesis_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, expression) = delimited(
        tuple((tag("("), multispace0)),
        expression,
        tuple((multispace0, tag(")"), )),
    )(i)?;
    let parenthesis = Parenthesis::new(expression);
    Ok((remaining_input, Expression::Parenthesis(parenthesis)))
}

fn prefix_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, ((pref, _), expression)) = tuple((
        alt((
            tuple((
                tag("+"),
                multispace0,
            )),
            tuple((
                tag("-"),
                multispace0,
            )),
            tuple((
                tag_no_case("PRIOR"),
                multispace1,
            )),
            tuple((
                tag_no_case("NOT"),
                multispace1,
            )),
        )),
        expression,
    ))(i)?;
    let prefix = bytes_to_string(pref).to_uppercase();
    let parenthesis = Prefix::new(prefix.as_str(), expression);
    Ok((remaining_input, Expression::Prefix(parenthesis)))
}

fn operator_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (expression_left, _, op_name, _, expression_right)) = tuple((
        terminal_expression,
        multispace0,
        alt((
            tag("+"),
            tag("-"),
            tag("/"),
            tag("*"),
            tag("||"),
            tag("="),
            tag("!="),
            tag("^="),
            tag("<>"),
            tag(">"),
            tag("<"),
            tag_no_case("like"),
            tag_no_case("exists"),
            tag_no_case("is"),
        )),
        multispace0,
        terminal_expression,
    ))(i)?;
    let operation_name = bytes_to_string(op_name).to_uppercase();
    let parenthesis = Operator::new(operation_name.as_str(), expression_left, expression_right);
    Ok((remaining_input, Expression::Operator(parenthesis)))
}

fn logical_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (expression_left, _, op_name, _, expression_right)) = tuple((
        operator_expression,
        multispace0,
        alt((
            tag_no_case("OR"),
            tag_no_case("AND"),
        )),
        multispace0,
        operator_expression,
    ))(i)?;
    let operation_name = bytes_to_string(op_name).to_uppercase();
    let parenthesis = Operator::new(operation_name.as_str(), expression_left, expression_right);
    Ok((remaining_input, Expression::Operator(parenthesis)))
}

fn function_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (func_name, _, _, _, vec_params, _, _, )) = tuple((
        take_while1(is_sql_identifier),
        multispace0,
        tag("("),
        multispace0,
        separated_list0(
            tuple((multispace0, tag(","), multispace0)),
            expression,
        ),
        multispace0,
        tag(")"),
    ))(i)?;
    let function_name = bytes_to_string(func_name).to_uppercase();
    let func = Function::new(function_name.as_str(), vec_params);
    Ok((remaining_input, Expression::Function(func)))
}

fn comparison_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, (expression_left, _, prec, _, op, _, _, _, vec_params, _, _, )) = tuple((
        terminal_expression,
        multispace1,
        alt((
            tag("="),
            tag("!="),
            tag(">"),
            tag("<"),
            tag("<="),
            tag(">="),
        )),
        multispace1,
        alt((
            tag_no_case("ANY"),
            tag_no_case("SOME"),
            tag_no_case("ALL"),
        )),
        multispace1,
        tag("("),
        multispace0,
        separated_list0(
            tuple((multispace0, tag(","), multispace0)),
            terminal_expression,
        ),
        multispace0,
        tag(")"),
    ))(i)?;
    let operation_name = bytes_to_string(op).to_uppercase();
    let preceded = bytes_to_string(prec);
    let func = ComparisonOperator::new(preceded.as_str(), operation_name.as_str(), expression_left, vec_params);
    Ok((remaining_input, Expression::ComparisonOperator(func)))
}


pub fn terminal_expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, expression) = alt((
        prefix_expression,
        sequence_expression,
        parenthesis_expression,
        rownum_expression,
        null_expression,
        string_expression,
        column_expression,
        number_expression,
    ))(i)?;
    Ok((remaining_input, expression))
}

pub fn expression(i: &[u8]) -> IResult<&[u8], Expression> {
    let (remaining_input, expression) = alt((
        function_expression,
        comparison_expression,
        logical_expression,
        operator_expression,
        terminal_expression,
    ))(i)?;
    Ok((remaining_input, expression))
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Comment {
    text: String,
}

impl fmt::Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "-- {}", self.text)?;
        Ok(())
    }
}

impl Comment {
    pub fn new(text_value: &str) -> Comment {
        Comment {
            text: text_value.to_string()
        }
    }
}

pub fn comment(i: &[u8]) -> IResult<&[u8], Comment> {
    let (remaining_input, data) = preceded(
        tag("--"),
        take_while(|c| c != b'\n'),
    )(i)?;
    let is_windows_end_line = data.last()
        .map(|c| b'\r'.eq_ignore_ascii_case(c)).unwrap_or(false);
    if is_windows_end_line {
        let bytes = & data[0..data.len() - 1];
        let text = bytes_to_string(bytes);
        return Ok((remaining_input, Comment { text }))
    }
    let text = bytes_to_string(data);
    Ok((remaining_input, Comment { text }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comments() {
        assert_eq!(comment(b"-- TEST\n").unwrap().1, Comment::new(" TEST"));
        assert_eq!(comment(b"-- TEST\r\n").unwrap().1, Comment::new(" TEST"));
        assert_eq!(comment(b"--\n").unwrap().1, Comment::new(""));
    }

    #[test]
    fn expressions() {
        assert_eq!(expression(b"NUll").unwrap().1, Expression::Null);
        assert_eq!(expression(b"rownum").unwrap().1, Expression::Rownum);
        assert_eq!(expression(b"'essai'").unwrap().1, Expression::String("essai".to_string()));
        assert_eq!(expression(b"1").unwrap().1, Expression::Number(1, 0));
        assert_eq!(expression(b"+1").unwrap().1, Expression::Prefix(
            Prefix::new("+",
                        Expression::Number(1, 0),
            )));

        assert_eq!(expression(b"2+1").unwrap().1, Expression::Operator(
            Operator::new("+",
                          Expression::Number(2, 0),
                          Expression::Number(1, 0),
            )));
        assert_eq!(expression(b"-2.5").unwrap().1, Expression::Prefix(
            Prefix::new("-",
                        Expression::Number(2, 5),
            )));
        assert_eq!(expression(b"( 2.5)").unwrap().1, Expression::Parenthesis(
            Parenthesis::new(Expression::Number(2, 5))));

        assert_eq!(expression(b"1.2").unwrap().1, Expression::Number(1, 2));
        assert_eq!(expression(b"masequence.NEXTVAL").unwrap().1, Expression::SequenceNextval("masequence".to_string()));
        assert_eq!(expression(b"masequence.CURRVAL").unwrap().1, Expression::SequenceCurval("masequence".to_string()));
        assert_eq!(expression(b"sch.ta.colum").unwrap().1, Expression::Column(ColumnExpression::new_with_schema(
            "colum",
            "ta",
            "sch")
        ));
        assert_eq!(expression(b"ta.colum").unwrap().1, Expression::Column(ColumnExpression::new_with_table(
            "colum",
            "ta")
        ));

        assert_eq!(expression(b"colum").unwrap().1, Expression::Column(ColumnExpression::new(
            "colum")
        ));

        assert_eq!(expression(b"maColonne is not null").unwrap().1,
                   Expression::Operator(Operator::new("IS",
                                                      Expression::Column(ColumnExpression::new(
                                                          "maColonne")),
                                                      Expression::Prefix(
                                                          Prefix::new("NOT", Expression::Null))))
        );
        assert_eq!(expression(b"TO_DATE('2021-12-01', 'YYYY-MM-DD')").unwrap().1,
                   Expression::Function(Function::new("TO_DATE",
                                                      vec![
                                                          Expression::String("2021-12-01".to_string()),
                                                          Expression::String("YYYY-MM-DD".to_string()),
                                                      ]))
        );
        assert_eq!(expression(b"maColonne is not null AND b=2").unwrap().1,
                   Expression::Operator(Operator::new("AND",
                                                      Expression::Operator(Operator::new("IS",
                                                                                         Expression::Column(ColumnExpression::new(
                                                                                             "maColonne")),
                                                                                         Expression::Prefix(
                                                                                             Prefix::new("NOT", Expression::Null)))),
                                                      Expression::Operator(Operator::new("=",
                                                                                         Expression::Column(ColumnExpression::new(
                                                                                             "b")),
                                                                                         Expression::Number(2, 0)))))
        );
    }

    #[test]
    fn sql_identifiers() {
        let id1 = b"foo";
        let id2 = b"f_o_o";
        let id3 = b"foo12";
        let id4 = b":fo oo";
        let id5 = b" primary ";
        let id6 = b"`primary`";

        assert!(sql_identifier(id1).is_ok());
        assert!(sql_identifier(id2).is_ok());
        assert!(sql_identifier(id3).is_ok());
        assert!(sql_identifier(id4).is_err());
        assert!(sql_identifier(id5).is_err());
        assert!(sql_identifier(id6).is_ok());
    }
}

pub fn bytes_to_string(bytes: &[u8]) -> String {
    std::str::from_utf8(bytes).unwrap().to_string()
}

use std::fmt;

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while1};
use nom::character::complete::{alphanumeric1, multispace0, multispace1};
use nom::combinator::opt;
use nom::IResult;
use nom::sequence::tuple;

use crate::table::bytes_to_string;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Salt {
    None,
    Salt,
    NoSalt,
}

impl fmt::Display for Salt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Salt::Salt => write!(f, " SALT")?,
            Salt::NoSalt => write!(f, " NO SALT")?,
            Salt::None => write!(f, "")?
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct EncryptionSpec {
    encrypt_algorithm: Option<String>,
    password: Option<String>,
    salt_type: Salt,
}

impl fmt::Display for EncryptionSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.encrypt_algorithm.is_some() {
            write!(f, " USING '{}'", self.encrypt_algorithm.as_ref().unwrap())?;
        }
        if self.password.is_some() {
            write!(f, " IDENTIFIED BY {}", self.password.as_ref().unwrap())?;
        }
        write!(f, "{}", self.salt_type)?;
        Ok(())
    }
}

fn salt(i: &[u8]) -> IResult<&[u8], Salt> {
    let res: IResult<&[u8], (&[u8], &[u8])> = tuple((
        multispace0,
        alt((
            tag_no_case("SALT"),
            tag_no_case("NO SALT")
        ))
    ))(i);
    res.map(|(remaining_input, t)| {
        let typ = bytes_to_string(t.1).to_uppercase();
        match typ.as_str() {
            "SALT" => Ok((remaining_input, Salt::Salt)),
            "NO SALT" => Ok((remaining_input, Salt::NoSalt)),
            _ => Ok((remaining_input, Salt::None)),
        }
    }).unwrap_or(Ok((i, Salt::None)))
}

pub fn encryption_spec(i: &[u8]) -> IResult<&[u8], EncryptionSpec> {
    let (remaining_input, (_, opt_ea, opt_p, salt_type)) = tuple((
        multispace0,
        opt(tuple((
            tag_no_case("USING"),
            multispace1,
            tag("'"),
            take_while1(|c| c != b'\''),
            tag("'"),
            multispace0
        ))),
        opt(tuple((
            tag_no_case("IDENTIFIED"),
            multispace1,
            tag_no_case("BY"),
            multispace1,
            alphanumeric1,
            multispace0
        ))
        ),
        salt
    ))(i)?;
    let encrypt_algorithm = opt_ea.map(|t| bytes_to_string(t.3));
    let password = opt_p.map(|t| bytes_to_string(t.4));
    Ok((remaining_input, EncryptionSpec { salt_type, password, encrypt_algorithm }))
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_encryption_spec() {
       assert_eq!( encryption_spec(b" USING 'test'").unwrap().1, EncryptionSpec{
           encrypt_algorithm:Some("test".to_string()),
           salt_type:Salt::None,
           password:None
       });
        assert_eq!( encryption_spec(b"Identified by truc").unwrap().1, EncryptionSpec{
            encrypt_algorithm:None,
            salt_type:Salt::None,
            password:Some("truc".to_string())
        });

        assert_eq!( encryption_spec(b"Salt").unwrap().1, EncryptionSpec{
            encrypt_algorithm:None,
            salt_type:Salt::Salt,
            password:None
        });
    }

    #[test]
    fn test_salt() {
        assert_eq!(salt(b"SALT").unwrap().1, Salt::Salt);
        assert_eq!(salt(b"No salt").unwrap().1, Salt::NoSalt);
        assert_eq!(salt(b"").unwrap().1, Salt::None);
    }
}
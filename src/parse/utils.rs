use crate::parse::{
  base,
  error::{
    ParseError,
    ParseErrorKind,
  },
  span::Span,
};

use nom::{
  bytes::complete::{
    tag,
    take_till,
    take_till1,
  },
  character::complete::{
    space0,
    space1,
  },
  multi::many0,
  sequence::terminated,
  IResult,
};

use alloc::string::String;
use num_bigint::BigUint;

pub fn parse_integer(from: Span) -> IResult<Span, BigUint, ParseError<Span>> {
  let base = base::LitBase::Dec;
  let (i, digits) = base::parse_litbase_digits(base)(from)?;
  match base_x::decode(base.base_digits(), &digits) {
    Ok(bytes) => Ok((i, BigUint::from_bytes_be(&bytes))),
    Err(_) => Err(nom::Err::Error(ParseError::new(
      i,
      ParseErrorKind::InvalidBaseEncoding(base),
    ))),
  }
}

pub fn parse_u64(from: Span) -> IResult<Span, u64, ParseError<Span>> {
  let base = base::LitBase::Dec;
  let (i, digits) = base::parse_litbase_digits(base)(from)?;
  match base_x::decode(base.base_digits(), &digits) {
    Ok(bytes) => {
      use ParseErrorKind::ParseIntErr;
      let x = u64::from_str_radix(&digits, base.radix()).map_or_else(
        |e| Err(nom::Err::Error(ParseError::new(from, ParseIntErr(e)))),
        Ok,
      )?;
      Ok((i, x))
    }
    Err(_) => Err(nom::Err::Error(ParseError::new(
      i,
      ParseErrorKind::InvalidBaseEncoding(base),
    ))),
  }
}

pub fn parse_text(from: Span) -> IResult<Span, String, ParseError<Span>> {
  let (i, s) = take_till1(|x| char::is_whitespace(x))(from)?;
  let s: String = String::from(s.fragment().clone());
  Ok((i, s))
}

pub fn parse_space(i: Span) -> IResult<Span, (), ParseError<Span>> {
  let (i, _) = space0(i)?;
  Ok((i, ()))
}
pub fn parse_space1(i: Span) -> IResult<Span, (), ParseError<Span>> {
  let (i, _) = space1(i)?;
  Ok((i, ()))
}

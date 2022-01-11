use crate::parse::{
  base,
  base::parse_multibase,
  error::{
    ParseError,
    ParseErrorKind,
  },
  span::Span,
};
use sp_cid::Cid;
use sp_std::convert::TryFrom;

use nom::{
  bytes::complete::{
    tag,
    take_till,
    take_till1,
  },
  character::complete::{
    multispace0,
    multispace1,
  },
  multi::many0,
  sequence::terminated,
  Err,
  IResult,
};

use alloc::string::String;
use num_bigint::BigUint;
use sp_std::vec::Vec;

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
    Ok(_) => {
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

pub fn parse_line_comment(i: Span) -> IResult<Span, Span, ParseError<Span>> {
  let (i, _) = tag("--")(i)?;
  let (i, com) = take_till(|c| c == '\n')(i)?;
  Ok((i, com))
}
pub fn parse_space(i: Span) -> IResult<Span, Vec<Span>, ParseError<Span>> {
  let (i, _) = multispace0(i)?;
  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
  Ok((i, com))
}

pub fn parse_space1(i: Span) -> IResult<Span, Vec<Span>, ParseError<Span>> {
  let (i, _) = multispace1(i)?;
  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
  Ok((i, com))
}

pub fn parse_cid(from: Span) -> IResult<Span, Cid, ParseError<Span>> {
  let (upto, (_, bytes)) = parse_multibase()(from)?;
  match Cid::try_from(bytes) {
    Ok(cid) => Ok((upto, cid)),
    Err(_) => Err(Err::Error(ParseError::new(upto, ParseErrorKind::CidError))),
  }
}

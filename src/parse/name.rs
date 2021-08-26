use num_bigint::BigUint;

use crate::{
  name::{
    Name,
    NamePart,
  },
  parse::{
    base::{
      parse_litbase_digits,
      LitBase,
    },
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
  },
};
use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till,
    take_till1,
  },
  character::complete::{
    multispace0,
    multispace1,
  },
  combinator::{
    eof,
    peek,
    value,
  },
  multi::many0,
  sequence::terminated,
  Err,
  IResult,
};

use sp_cid::Cid;
use sp_im::vector::Vector;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
};
use sp_multihash::{
  Code,
  MultihashDigest,
};

use alloc::string::String;

use sp_std::{
  borrow::ToOwned,
  vec::Vec,
};

pub fn parse_namepart_text(
  from: Span,
) -> IResult<Span, NamePart, ParseError<Span>> {
  let (i, s) = take_till1(|x| char::is_whitespace(x) | (x == '.'))(from)?;
  let s: String = String::from(s.fragment().to_owned());
  Ok((i, NamePart::Text(s)))
}

pub fn parse_namepart_int(
  from: Span,
) -> IResult<Span, NamePart, ParseError<Span>> {
  let (i, digits) = parse_litbase_digits(LitBase::Dec)(from)?;
  match base_x::decode(LitBase::Dec.base_digits(), &digits) {
    Ok(bytes) => Ok((i, NamePart::Int(BigUint::from_bytes_be(&bytes)))),
    Err(_) => Err(nom::Err::Error(ParseError::new(
      i,
      ParseErrorKind::InvalidBaseEncoding(LitBase::Dec),
    ))),
  }
}

use crate::{
  name::Name,
  parse::{
    base,
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
    utils::{
      parse_integer,
      parse_space,
    },
  },
  universe::Universe,
};

use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till,
    take_till1,
  },
  character::complete::{
    digit1,
    multispace0,
    multispace1,
    satisfy,
  },
  combinator::{
    eof,
    opt,
    peek,
    value,
  },
  error::context,
  multi::{
    many0,
    many1,
  },
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  IResult,
  Parser,
};




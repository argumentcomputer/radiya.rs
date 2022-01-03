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

use num_bigint::BigUint;
use sp_im::vector::Vector;
use sp_std::boxed::Box;

pub type UnivCtx = Vector<Name>;

pub fn parse_succs_level(
  ctx: UnivCtx,
) -> impl Fn(Span) -> IResult<Span, Universe, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_universe(ctx.clone())(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("+")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, n) = parse_integer(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag(")")(i)?;
    let mut n = n;
    let mut u = u;
    while n > 0u64.into() {
      u = Universe::Succ(Box::new(u));
      n = n - BigUint::from(1u64)
    }
    Ok((i, u))
  }
}

pub fn parse_const_level()
-> impl Fn(Span) -> IResult<Span, Universe, ParseError<Span>> {
  move |i: Span| {
    let (i, n) = parse_integer(i)?;
    let mut n = n;
    let mut u = Universe::Zero;
    while n > 0u64.into() {
      u = Universe::Succ(Box::new(u));
      n = n - BigUint::from(1u64)
    }
    Ok((i, u))
  }
}

// pub fn parse_param(
//  ctx: UnivCtx,
//) -> impl Fn(Span) -> IResult<Span, Universe, ParseError<Span>> {
//  move |i: Span| {
//    let (i, _) = tag("(")(i)?;
//    let (i, _) = parse_space(i)?;
//    let (i, u) = parse_universe(ctx.clone())(i)?;
//    let (i, _) = tag("+")(i)?;
//    let (i, n) = parse_integer(i)?;
//    let (i, _) = parse_space(i)?;
//    let (i, _) = tag(")")(i)?;
//    let mut n = n;
//    let mut u = u;
//    while n > 0u64.into() {
//      u = Universe::Succ(Box::new(u));
//      n = n - BigUint::from(1u64)
//    }
//    Ok((i, u))
//  }
//}

pub fn parse_universe(
  ctx: UnivCtx,
) -> impl Fn(Span) -> IResult<Span, Universe, ParseError<Span>> {
  move |i: Span| {
    alt((
      context("const level", parse_const_level()),
      context("succs level", parse_succs_level(ctx.clone())),
      /* context("max", parse_max(defs.clone(), ctx.clone())),
       * context("param", parse_param(defs.to_owned(), ctx.clone())), */
    ))(i)
  }
}

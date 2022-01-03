// use crate::{
//  parse::{
//    base,
//    env::Defs,
//    error::{
//      ParseError,
//      ParseErrorKind,
//    },
//    span::Span,
//  },
//  term::{
//    Env,
//    Opr,
//    Term,
//  },
//};
// use sp_cid::Cid;
// use sp_multihash::{
//  Code,
//  MultihashDigest,
//};
// use sp_im::{
//  ordmap::OrdMap,
//  vector::Vector,
//};
// use num_bigint::BigUint;
//
// use base_x;
//
// use sp_ipld::{
//  dag_cbor::DagCborCodec,
//  Codec,
//};
// use sp_std::{
//  borrow::ToOwned,
//  boxed::Box,
//  cell::RefCell,
//  collections::btree_map::BTreeMap,
//  rc::Rc,
//  vec::Vec,
//};
// use alloc::string::{
//  String,
//  ToString,
//};
// use nom::{
//  branch::alt,
//  bytes::complete::{
//    tag,
//    take_till,
//    take_till1,
//  },
//  character::complete::{
//    digit1,
//    multispace0,
//    multispace1,
//    satisfy,
//  },
//  combinator::{
//    eof,
//    opt,
//    peek,
//    value,
//  },
//  error::context,
//  multi::{
//    many0,
//    many1,
//  },
//  sequence::{
//    delimited,
//    preceded,
//    terminated,
//  },
//  Err,
//  IResult,
//  Parser,
//};
// use sp_im::conslist::ConsList;
// use sp_std::collections::vec_deque::VecDeque;
//
// pub type Ctx = ConsList<String>;
//
// pub fn reserved_symbols() -> VecDeque<String> {
//  VecDeque::from(vec![
//    String::from("--"),
//    String::from("λ"),
//    String::from("μ"),
//    String::from("."),
//    String::from(":="),
//    String::from("("),
//    String::from(")"),
//    String::from("def"),
//  ])
//}
// pub fn parse_line_comment(i: Span) -> IResult<Span, Span, ParseError<Span>> {
//  let (i, _) = tag("--")(i)?;
//  let (i, com) = take_till(|c| c == '\n')(i)?;
//  Ok((i, com))
//}
// pub fn parse_space(i: Span) -> IResult<Span, Vec<Span>, ParseError<Span>> {
//  let (i, _) = multispace0(i)?;
//  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
//  Ok((i, com))
//}
// pub fn parse_space1(i: Span) -> IResult<Span, Vec<Span>, ParseError<Span>> {
//  let (i, _) = multispace1(i)?;
//  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
//  Ok((i, com))
//}
// pub fn is_numeric_symbol_string1(s: &str) -> bool {
//  s.starts_with('0')
//    || s.starts_with('1')
//    || s.starts_with('2')
//    || s.starts_with('3')
//    || s.starts_with('4')
//    || s.starts_with('5')
//    || s.starts_with('6')
//    || s.starts_with('7')
//    || s.starts_with('8')
//    || s.starts_with('9')
//}
// pub fn parse_name(from: Span) -> IResult<Span, String, ParseError<Span>> {
//  let (i, s) = take_till1(|x| {
//    char::is_whitespace(x) | (x == ')') | (x == '(') | (x == '.')
//  })(from)?;
//  let s: String = String::from(s.fragment().to_owned());
//  if reserved_symbols().contains(&s) {
//    Err(Err::Error(ParseError::new(from, ParseErrorKind::ReservedKeyword(s))))
//  }
//  else if is_numeric_symbol_string1(&s) {
//    Err(Err::Error(ParseError::new(from, ParseErrorKind::NumericSyntax(s))))
//  }
//  else {
//    Ok((i, s))
//  }
//}
// pub fn parse_var(
//  defs: Defs,
//  ctx: Ctx,
//) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
//  move |from: Span| {
//    let (upto, nam) = context("local or global reference", parse_name)(from)?;
//    if let Some((idx, _)) = ctx.iter().enumerate().find(|(_, x)| **x == nam) {
//      Ok((upto, Term::Var { idx: idx.into() }))
//    }
//    else if let Some(def) = defs.borrow().get(&nam) {
//      Ok((upto, Term::Ref { cid: *def }))
//    }
//    else {
//      Err(Err::Error(ParseError::new(
//        upto,
//        ParseErrorKind::UndefinedReference(nam.clone(), ctx.clone()),
//      )))
//    }
//  }
//}
// pub fn parse_tele_end(i: Span) -> IResult<Span, (), ParseError<Span>> {
//  let (i, _) = alt((peek(tag("def")), peek(tag(")")), peek(eof)))(i)?;
//  Ok((i, ()))
//}
// pub fn parse_args(
//  defs: Defs,
//  ctx: Ctx,
//) -> impl FnMut(Span) -> IResult<Span, Vec<Term>, ParseError<Span>> {
//  move |mut i: Span| {
//    let mut res = Vec::new();
//
//    loop {
//      match preceded(parse_space, peek(parse_tele_end))(i) {
//        Ok((i2, _)) => return Ok((i2, res)),
//        _ => {}
//      }
//      match preceded(parse_space, parse_term(defs.to_owned(), ctx.clone()))(i)
// {        Err(e) => return Err(e),
//        Ok((i2, x)) => {
//          res.push(x);
//          i = i2;
//        }
//      }
//    }
//  }
//}
// pub fn parse_telescope(
//  defs: Defs,
//  ctx: Ctx,
//) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
//  move |from: Span| {
//    let (i, fun) =
//      context("app fun", parse_term(defs.clone(), ctx.clone()))(from)?;
//    let (i, _) = parse_space(i)?;
//    let (upto, args) = parse_args(defs.clone(), ctx.clone())(i)?;
//    let trm = args.into_iter().fold(fun, |acc, arg| Term::App {
//      fun: Box::new(acc),
//      arg: Box::new(arg),
//    });
//    return Ok((upto, trm));
//  }
//}
// pub fn parse_lam(
//  defs: Defs,
//  ctx: Ctx,
//) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
//  move |from: Span| {
//    let (i, _) = tag("λ")(from)?;
//    let (i, _) = parse_space(i)?;
//    let (i, bs) = many1(terminated(parse_name, parse_space))(i)?;
//    let (i, _) = terminated(tag("."), parse_space)(i)?;
//    let mut ctx2 = ctx.clone();
//    for b in bs.iter() {
//      ctx2 = ctx2.cons(b.clone());
//    }
//    let (upto, bod) = parse_telescope(defs.to_owned(), ctx2)(i)?;
//    let trm =
//      bs.into_iter().rev().fold(bod, |acc, _| Term::Lam { bod: Box::new(acc)
// });    Ok((upto, trm))
//  }
//}
// pub fn parse_fix(
//  defs: Defs,
//  ctx: Ctx,
//) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
//  move |from: Span| {
//    let (i, _) = tag("μ")(from)?;
//    let (i, _) = parse_space(i)?;
//    let (i, n) = parse_name(i)?;
//    let (i, _) = parse_space(i)?;
//    let (i, _) = terminated(tag("."), parse_space)(i)?;
//    let mut ctx2 = ctx.clone();
//    ctx2 = ctx2.cons(n.clone());
//    let (upto, bod) = parse_telescope(defs.to_owned(), ctx2)(i)?;
//    Ok((upto, Term::Fix { bod: Box::new(bod) }))
//  }
//}
// pub fn parse_nat(from: Span) -> IResult<Span, Term, ParseError<Span>> {
//  let (i, base) = opt(preceded(tag("0"), base::parse_litbase_code()))(from)?;
//  let base = base.unwrap_or(base::LitBase::Dec);
//  let (upto, digits) = base::parse_litbase_digits(base)(i)?;
//  match base_x::decode(base.base_digits(), &digits) {
//    Ok(bytes) => Ok((upto, Term::Nat { nat: BigUint::from_bytes_be(&bytes)
// })),    Err(_) => Err(nom::Err::Error(ParseError::new(
//      upto,
//      ParseErrorKind::InvalidBaseEncoding(base),
//    ))),
//  }
//}
// pub fn parse_term(
//  defs: Defs,
//  ctx: Ctx,
//) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
//  move |i: Span| {
//    alt((
//      context(
//        "application telescope",
//        delimited(
//          preceded(tag("("), parse_space),
//          parse_telescope(defs.clone(), ctx.clone()),
//          preceded(parse_space, tag(")")),
//        ),
//      ),
//      context("lam", parse_lam(defs.clone(), ctx.clone())),
//      context("fix", parse_fix(defs.clone(), ctx.clone())),
//      // context("lit", parse_lit),
//      context("var", parse_var(defs.to_owned(), ctx.clone())),
//    ))(i)
//  }
//}
//
////#[cfg(test)]
//// pub mod tests {
////  use super::*;
////
////  #[test]
////  fn test_parse_apps() {
////    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
////      parse_telescope(Rc::new(RefCell::new(BTreeMap::new())),
//// ConsList::new())(        Span::new(i),
////      )
////    }
////    let res = test("1");
////    assert!(res.is_ok());
////    let res = test("1 1");
////    assert!(res.is_ok());
////    let res = test("1 1 def");
////    assert!(res.is_ok());
////  }
////
////  #[test]
////  fn test_parse_term() {
////    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
////      parse_term(Rc::new(RefCell::new(BTreeMap::new())), ConsList::new())(
////        Span::new(i),
////      )
////    }
////    let res = test("μx.x");
////    println!("{:?}", res);
////    assert!(res.is_ok());
////    let res = test("λx.x");
////    println!("{:?}", res);
////    assert!(res.is_ok());
////    let res = test("λx y z.x");
////    println!("{:?}", res);
////    assert!(res.is_ok());
////    let res = test("(+ 1 1)");
////    println!("{:?}", res);
////    assert!(res.is_ok());
////  }
//// }

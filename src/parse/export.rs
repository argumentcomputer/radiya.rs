use crate::{
  export::{
    Ctx,
    Eid,
    ExprDecl,
    NameDecl,
    Nid,
    Uid,
    UnivDecl,
  },
  parse::{
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
    utils::{
      parse_integer,
      parse_space1,
      parse_text,
      parse_u64,
    },
  },
};

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  cell::RefCell,
  rc::Rc,
  vec::Vec,
};

use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till,
  },
  character::complete::{
    multispace0,
    multispace1,
  },
  combinator::map,
  multi::many0,
  number::{
    complete::u64,
    Endianness,
  },
  sequence::terminated,
  Err,
  IResult,
  Parser,
};

use alloc::string::{
  String,
  ToString,
};

type RefCtx = Rc<RefCell<Ctx>>;

pub fn parse_nid(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Nid, ParseError<Span>> {
  move |from: Span| {
    let (i, nid) = map(parse_u64, Nid)(from)?;
    let ctx = ctx.borrow();
    if let None = ctx.names.get(&nid) {
      return Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::UndefinedNid(nid),
      )));
    }
    else {
      return Ok((i, nid));
    }
  }
}

pub fn parse_eid(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Eid, ParseError<Span>> {
  move |from: Span| {
    let (i, eid) = map(parse_u64, Eid)(from)?;
    let ctx = ctx.borrow();
    if let None = ctx.exprs.get(&eid) {
      return Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::UndefinedEid(eid),
      )));
    }
    else {
      return Ok((i, eid));
    }
  }
}

pub fn parse_uid(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Uid, ParseError<Span>> {
  move |from: Span| {
    let (i, uid) = map(parse_u64, Uid)(from)?;
    let ctx = ctx.borrow();
    if let None = ctx.univs.get(&uid) {
      return Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::UndefinedUid(uid),
      )));
    }
    else {
      return Ok((i, uid));
    }
  }
}

pub fn parse_prim_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, (), ParseError<Span>> {
  move |i: Span| {
    let (i, id) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, tag) = alt((tag("#N"), tag("#U"), tag("#E")))(i)?;
    match tag.fragment().to_owned() {
      "#N" => {
        let ctx_ref = ctx.borrow();
        if let Some(_) = ctx_ref.names.get(&Nid(id)) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedNid(Nid(id)),
          )));
        }
        else {
          let (i, decl) = alt((
            parse_name_string_decl(ctx.clone()),
            parse_name_int_decl(ctx.clone()),
          ))(i)?;
          let mut ctx_mut = ctx.borrow_mut();
          ctx_mut.names.insert(Nid(id), decl);
          Ok((i, ()))
        }
      }
      _ => todo!(),
    }
  }
}

pub fn parse_name_string_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, NameDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("S")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, prev) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_text(i)?;
    return Ok((i, NameDecl::NameStr { prev, name }));
  }
}

pub fn parse_name_int_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, NameDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("I")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, prev) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_integer(i)?;
    return Ok((i, NameDecl::NameInt { prev, name }));
  }
}

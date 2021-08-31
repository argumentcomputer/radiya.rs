use crate::{
  export::{
    Ctx,
    Eid,
    ExprDecl,
    NameDecl,
    Nid,
    Notation,
    TopDecl,
    Uid,
    UnivDecl,
  },
  expr::Bind,
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

use sp_im::vector::Vector;

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
  combinator::{
    map,
    value,
  },
  multi::many0,
  number::{
    complete::u64,
    Endianness,
  },
  sequence::{
    preceded,
    terminated,
  },
  Err,
  IResult,
  Parser,
};

use alloc::string::{
  String,
  ToString,
};

type RefCtx = Rc<RefCell<Ctx>>;

/// Every line of the export file defines a command
pub enum Command {
  Univ(Uid, UnivDecl),
  Name(Nid, NameDecl),
  Expr(Eid, ExprDecl),
  Notation(Notation),
  Top(TopDecl),
}

pub fn parse_export(from: Span) -> IResult<Span, Ctx, ParseError<Span>> {
  let ctx = Rc::new(RefCell::new(Ctx::new()));
  let mut i = from;
  loop {
    let (i2, command) = parse_command(ctx.clone())(i)?;
    match command {
      Command::Name(nid, decl) => {
        let mut ctx_mut = ctx.borrow_mut();
        if let Some(_) = ctx_mut.names.get(&nid) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedNid(nid),
          )));
        }
        else {
          ctx_mut.names.insert(nid, decl);
        }
      }
      Command::Univ(uid, decl) => {
        let mut ctx_mut = ctx.borrow_mut();
        if let Some(_) = ctx_mut.univs.get(&uid) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedUid(uid),
          )));
        }
        else {
          ctx_mut.univs.insert(uid, decl);
        }
      }
      Command::Expr(eid, decl) => {
        let mut ctx_mut = ctx.borrow_mut();
        if let Some(_) = ctx_mut.exprs.get(&eid) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedEid(eid),
          )));
        }
        else {
          ctx_mut.exprs.insert(eid, decl);
        }
      }
      _ => todo!(),
    }
  }
}

/// Parses a Command
pub fn parse_command(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |from: Span| {
    alt((
      parse_top_decl(ctx.clone()),
      parse_notation_decl(ctx.clone()),
      parse_name_decl(ctx.clone()),
      parse_univ_decl(ctx.clone()),
      parse_expr_decl(ctx.clone()),
    ))(from)
  }
}

pub fn parse_top_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, res) = alt((
      parse_definition(ctx.clone()),
      parse_inductive(ctx.clone()),
      parse_axiom(ctx.clone()),
      parse_quot(),
    ))(i)?;
    return Ok((i, Command::Top(res)));
  }
}

/// The syntax for TopDecl::Definition is
/// ```text
/// #DEF <nidx> <eidx_1> <edix_2> <nidx*>
/// ```
pub fn parse_definition(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, TopDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#DEF")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, val) = parse_eid(ctx.clone())(i)?;
    let (i, levels) = many0(preceded(parse_space1, parse_nid(ctx.clone())))(i)?;
    let levels = Vector::from(levels);
    return Ok((i, TopDecl::Definition { name, typ, val, levels }));
  }
}

pub fn parse_inductive(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, TopDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#IND")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, num_params) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, num_intros) = parse_u64(i)?;
    let mut intros: Vec<(Nid, Eid)> = Vec::new();
    let mut i1 = i;
    for _ in 0..num_intros {
      let (i2, _) = parse_space1(i1)?;
      let (i2, intro_nam) = parse_nid(ctx.clone())(i2)?;
      let (i2, _) = parse_space1(i2)?;
      let (i2, intro_typ) = parse_eid(ctx.clone())(i2)?;
      intros.push((intro_nam, intro_typ));
      i1 = i2
    }
    let (i, levels) = many0(preceded(parse_space1, parse_nid(ctx.clone())))(i)?;
    let intros = Vector::from(intros);
    let levels = Vector::from(levels);
    return Ok((i, TopDecl::Inductive {
      num_params,
      name,
      typ,
      intros,
      levels,
    }));
  }
}

pub fn parse_quot() -> impl Fn(Span) -> IResult<Span, TopDecl, ParseError<Span>>
{
  move |i: Span| {
    let (i, _) = tag("#QUOT")(i)?;
    return Ok((i, TopDecl::Quotient));
  }
}

pub fn parse_axiom(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, TopDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#AX")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eid(ctx.clone())(i)?;
    let (i, levels) = many0(preceded(parse_space1, parse_nid(ctx.clone())))(i)?;
    let levels = Vector::from(levels);
    return Ok((i, TopDecl::Axiom { name, typ, levels }));
  }
}

pub fn parse_notation_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, tag) = alt((tag("#PREFIX"), tag("#INFIX"), tag("#POSTFIX")))(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, prec) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, token) = parse_text(i)?;
    let res = match tag.fragment().to_owned() {
      "#PREFIX" => Notation::Prefix { name, prec, token },
      "#INFIX" => Notation::Infix { name, prec, token },
      "#POSTFIX" => Notation::Postfix { name, prec, token },
      _ => unreachable!(),
    };
    return Ok((i, Command::Notation(res)));
  }
}

/// Parses a Command::Name
pub fn parse_name_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, id) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, _) = tag("#N")(i)?;
    let (i, decl) =
      alt((parse_name_string(ctx.clone()), parse_name_int(ctx.clone())))(i)?;
    Ok((i, Command::Name(Nid(id), decl)))
  }
}

/// The syntax of the NameDecl::NameStr command is
/// ```text
/// <nidx'> #NS <nidx> <string>
/// ```
pub fn parse_name_string(
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

/// The syntax of the NameDecl::NameInt command is
/// ```text
/// <nidx'> #NI <nidx> <integer>
/// ```
pub fn parse_name_int(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, NameDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("I")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, prev) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, int) = parse_integer(i)?;
    return Ok((i, NameDecl::NameInt { prev, int }));
  }
}

/// Parses a Command::Univ
pub fn parse_univ_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, id) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, _) = tag("#U")(i)?;
    let (i, decl) = alt((
      parse_univ_succ(ctx.clone()),
      parse_univ_max(ctx.clone()),
      parse_univ_imax(ctx.clone()),
      parse_univ_param(ctx.clone()),
    ))(i)?;
    Ok((i, Command::Univ(Uid(id), decl)))
  }
}

/// The syntax for the UnivDecl::UnivSucc command is:
/// ```text
/// <uidx'> #US <uidx>
/// ```
pub fn parse_univ_succ(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, UnivDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("S")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, pred) = parse_uid(ctx.clone())(i)?;
    return Ok((i, UnivDecl::UnivSucc { pred }));
  }
}

/// The syntax for the UnivDecl::UnivMax command is:
/// ```text
/// <uidx'> #UM <uidx_1> <uidx_2>
/// ```
pub fn parse_univ_max(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, UnivDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("M")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, lhs) = parse_uid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, rhs) = parse_uid(ctx.clone())(i)?;
    return Ok((i, UnivDecl::UnivMax { lhs, rhs }));
  }
}

/// The syntax for the UnivDecl::UnivIMax command is:
/// ```text
/// <uidx'> #UIM <uidx_1> <uidx_2>
/// ```
pub fn parse_univ_imax(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, UnivDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("M")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, lhs) = parse_uid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, rhs) = parse_uid(ctx.clone())(i)?;
    return Ok((i, UnivDecl::UnivIMax { lhs, rhs }));
  }
}

/// The syntax for the UnivDecl::UnivParam command is:
/// ```text
/// <uidx'> #UP <nidx>
/// ```
pub fn parse_univ_param(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, UnivDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("P")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    return Ok((i, UnivDecl::UnivParam { name }));
  }
}

/// Parses a Command::Expr
pub fn parse_expr_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, id) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, _) = tag("#E")(i)?;
    let (i, decl) = alt((
      parse_expr_var(),
      parse_expr_sort(ctx.clone()),
      parse_expr_const(ctx.clone()),
      parse_expr_app(ctx.clone()),
      parse_expr_lam(ctx.clone()),
      parse_expr_pi(ctx.clone()),
      parse_expr_let(ctx.clone()),
    ))(i)?;
    Ok((i, Command::Expr(Eid(id), decl)))
  }
}

/// The syntax for the ExprDecl::ExprVar command is:
/// ```text
/// <eidx'> #EV <integer>
/// ```
pub fn parse_expr_var()
-> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("V")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, idx) = parse_u64(i)?;
    return Ok((i, ExprDecl::ExprVar { idx }));
  }
}

/// The syntax for the ExprDecl::ExprSort command is:
/// ```text
/// <eidx'> #ES <uidx>
/// ```
pub fn parse_expr_sort(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("V")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, univ) = parse_uid(ctx.clone())(i)?;
    return Ok((i, ExprDecl::ExprSort { univ }));
  }
}

/// The syntax for the ExprDecl::ExprConst command is:
/// ```text
/// <eidx'> #EC <nidx> <uidx>*
/// ```
pub fn parse_expr_const(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("C")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, levels) = many0(preceded(parse_space1, parse_uid(ctx.clone())))(i)?;
    return Ok((i, ExprDecl::ExprConst { name, levels: Vector::from(levels) }));
  }
}

/// The syntax for the ExprDecl::ExprApp command is:
/// ```text
/// <eidx'> #EA <eidx_1> <eidx_2>
/// ```
pub fn parse_expr_app(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("A")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, fun) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, arg) = parse_eid(ctx.clone())(i)?;
    return Ok((i, ExprDecl::ExprApp { fun, arg }));
  }
}

/// The syntax for the ExprDecl::ExprLam command is:
/// ```text
/// <eidx'> #EL <info> <nidx> <eidx_1> <eidx_2>
/// ```
pub fn parse_expr_lam(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("L")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, info) = parse_bind(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, bod) = parse_eid(ctx.clone())(i)?;
    return Ok((i, ExprDecl::ExprLam { info, name, typ, bod }));
  }
}

/// The syntax for the ExprDecl::ExprLam command is:
/// ```text
/// <eidx'> #EL <info> <nidx> <eidx_1> <eidx_2>
/// ```
pub fn parse_expr_pi(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("P")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, info) = parse_bind(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, bod) = parse_eid(ctx.clone())(i)?;
    return Ok((i, ExprDecl::ExprPi { info, name, typ, bod }));
  }
}

/// The syntax for the ExprDecl::ExprLet command is:
/// ```text
/// <eidx'> #EZ <nidx> <eidx_1> <eidx_2> <eidx_3>
/// ```
pub fn parse_expr_let(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, ExprDecl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("Z")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, val) = parse_eid(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, bod) = parse_eid(ctx.clone())(i)?;
    return Ok((i, ExprDecl::ExprLet { name, typ, val, bod }));
  }
}

pub fn parse_bind(i: Span) -> IResult<Span, Bind, ParseError<Span>> {
  alt((
    value(Bind::Default, tag("#BD")),
    value(Bind::Implicit, tag("#BI")),
    value(Bind::Strict, tag("#BS")),
    value(Bind::Class, tag("#BC")),
  ))(i)
}

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

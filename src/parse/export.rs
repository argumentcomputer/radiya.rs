use crate::{
  export::{
    Ctx,
    Decl,
    EIdx,
    Expr,
    NIdx,
    Name,
    Notation,
    UIdx,
    Univ,
  },
  expression::Bind,
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
  cell::RefCell,
  rc::Rc,
  vec::Vec,
};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::newline,
  combinator::{
    eof,
    map,
    value,
  },
  multi::many0,
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

/// Every line of the export file defines a command
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Command {
  Univ(UIdx, Univ),
  Name(NIdx, Name),
  Expr(EIdx, Expr),
  Decl(Decl),
}

type RefCtx = Rc<RefCell<Ctx>>;

pub fn parse_export(from: Span) -> IResult<Span, Ctx, ParseError<Span>> {
  let ctx = Rc::new(RefCell::new(Ctx::new()));
  let mut i = from;
  loop {
    let (i2, command) = parse_command(ctx.clone())(i)?;
    match command {
      Command::Name(nidx, decl) => {
        let mut ctx_mut = ctx.borrow_mut();
        if let Some(_) = ctx_mut.names.get(&nidx) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedNIdx(nidx),
          )));
        }
        else {
          ctx_mut.names.insert(nidx, decl);
        }
      }
      Command::Univ(uidx, decl) => {
        let mut ctx_mut = ctx.borrow_mut();
        if let Some(_) = ctx_mut.univs.get(&uidx) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedUIdx(uidx),
          )));
        }
        else {
          ctx_mut.univs.insert(uidx, decl);
        }
      }
      Command::Expr(eidx, decl) => {
        let mut ctx_mut = ctx.borrow_mut();
        if let Some(_) = ctx_mut.exprs.get(&eidx) {
          return Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::RedefinedEIdx(eidx),
          )));
        }
        else {
          ctx_mut.exprs.insert(eidx, decl);
        }
      }
      _ => todo!(),
    }
    let (i2, _) = newline(i2)?;
    i = i2;
    if let Ok((i, _)) = eof::<Span, ParseError<Span>>(i) {
      return Ok((i, ctx.borrow().clone()));
    }
  }
}

/// Parses a Command
pub fn parse_command(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |from: Span| {
    alt((
      parse_decl(ctx.clone()),
      parse_name(ctx.clone()),
      parse_univ(ctx.clone()),
      parse_expr(ctx.clone()),
    ))(from)
  }
}

pub fn parse_decl(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, res) = alt((
      parse_definition(ctx.clone()),
      parse_inductive(ctx.clone()),
      parse_axiom(ctx.clone()),
      parse_notation(ctx.clone()),
      parse_quot(),
    ))(i)?;
    return Ok((i, Command::Decl(res)));
  }
}

/// The syntax for Decl::Definition is
/// ```text
/// #DEF <nidx> <eidx_1> <edix_2> <nidx*>
/// ```
pub fn parse_definition(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Decl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#DEF")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, val) = parse_eidx(ctx.clone())(i)?;
    let (i, levels) =
      many0(preceded(parse_space1, parse_nidx(ctx.clone())))(i)?;
    let levels = Vector::from(levels);
    return Ok((i, Decl::Definition { name, typ, val, levels }));
  }
}

pub fn parse_inductive(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Decl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#IND")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, num_params) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, num_intros) = parse_u64(i)?;
    let mut intros: Vec<(NIdx, EIdx)> = Vec::new();
    let mut i1 = i;
    for _ in 0..num_intros {
      let (i2, _) = parse_space1(i1)?;
      let (i2, intro_nam) = parse_nidx(ctx.clone())(i2)?;
      let (i2, _) = parse_space1(i2)?;
      let (i2, intro_typ) = parse_eidx(ctx.clone())(i2)?;
      intros.push((intro_nam, intro_typ));
      i1 = i2
    }
    let (i, levels) =
      many0(preceded(parse_space1, parse_nidx(ctx.clone())))(i)?;
    let intros = Vector::from(intros);
    let levels = Vector::from(levels);
    return Ok((i, Decl::Inductive { num_params, name, typ, intros, levels }));
  }
}

pub fn parse_quot() -> impl Fn(Span) -> IResult<Span, Decl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#QUOT")(i)?;
    return Ok((i, Decl::Quotient));
  }
}

pub fn parse_axiom(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Decl, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("#AX")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eidx(ctx.clone())(i)?;
    let (i, levels) =
      many0(preceded(parse_space1, parse_nidx(ctx.clone())))(i)?;
    let levels = Vector::from(levels);
    return Ok((i, Decl::Axiom { name, typ, levels }));
  }
}

pub fn parse_notation(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Decl, ParseError<Span>> {
  move |i: Span| {
    let (i, tag) = alt((tag("#PREFIX"), tag("#INFIX"), tag("#POSTFIX")))(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
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
    return Ok((i, Decl::Notation(res)));
  }
}

/// Parses a Command::Name
pub fn parse_name(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Command, ParseError<Span>> {
  move |i: Span| {
    let (i, id) = parse_u64(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, _) = tag("#N")(i)?;
    let (i, decl) =
      alt((parse_name_string(ctx.clone()), parse_name_int(ctx.clone())))(i)?;
    Ok((i, Command::Name(NIdx(id), decl)))
  }
}

/// The syntax of the Name::Str command is
/// ```text
/// <nidx'> #NS <nidx> <string>
/// ```
pub fn parse_name_string(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Name, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("S")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, prev) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_text(i)?;
    return Ok((i, Name::Str { prev, name }));
  }
}

/// The syntax of the Name::Int command is
/// ```text
/// <nidx'> #NI <nidx> <integer>
/// ```
pub fn parse_name_int(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Name, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("I")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, prev) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, int) = parse_integer(i)?;
    return Ok((i, Name::Int { prev, int }));
  }
}

/// Parses a Command::Univ
pub fn parse_univ(
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
    Ok((i, Command::Univ(UIdx(id), decl)))
  }
}

/// The syntax for the Univ::Succ command is:
/// ```text
/// <uidx'> #US <uidx>
/// ```
pub fn parse_univ_succ(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Univ, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("S")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, pred) = parse_uidx(ctx.clone())(i)?;
    return Ok((i, Univ::Succ { pred }));
  }
}

/// The syntax for the Univ::Max command is:
/// ```text
/// <uidx'> #UM <uidx_1> <uidx_2>
/// ```
pub fn parse_univ_max(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Univ, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("M")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, lhs) = parse_uidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, rhs) = parse_uidx(ctx.clone())(i)?;
    return Ok((i, Univ::Max { lhs, rhs }));
  }
}

/// The syntax for the Univ::IMax command is:
/// ```text
/// <uidx'> #UIM <uidx_1> <uidx_2>
/// ```
pub fn parse_univ_imax(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Univ, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("IM")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, lhs) = parse_uidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, rhs) = parse_uidx(ctx.clone())(i)?;
    return Ok((i, Univ::IMax { lhs, rhs }));
  }
}

/// The syntax for the Univ::Param command is:
/// ```text
/// <uidx'> #UP <nidx>
/// ```
pub fn parse_univ_param(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Univ, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("P")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    return Ok((i, Univ::Param { name }));
  }
}

/// Parses a Command::Expr
pub fn parse_expr(
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
    Ok((i, Command::Expr(EIdx(id), decl)))
  }
}

/// The syntax for the Expr::Var command is:
/// ```text
/// <eidx'> #EV <integer>
/// ```
pub fn parse_expr_var() -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>>
{
  move |i: Span| {
    let (i, _) = tag("V")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, idx) = parse_u64(i)?;
    return Ok((i, Expr::Var { idx }));
  }
}

/// The syntax for the ExprDecl::ExprSort command is:
/// ```text
/// <eidx'> #ES <uidx>
/// ```
pub fn parse_expr_sort(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("S")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, univ) = parse_uidx(ctx.clone())(i)?;
    return Ok((i, Expr::Sort { univ }));
  }
}

/// The syntax for the ExprDecl::ExprConst command is:
/// ```text
/// <eidx'> #EC <nidx> <uidx>*
/// ```
pub fn parse_expr_const(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("C")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, levels) =
      many0(preceded(parse_space1, parse_uidx(ctx.clone())))(i)?;
    return Ok((i, Expr::Const { name, levels: Vector::from(levels) }));
  }
}

/// The syntax for the ExprDecl::ExprApp command is:
/// ```text
/// <eidx'> #EA <eidx_1> <eidx_2>
/// ```
pub fn parse_expr_app(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("A")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, fun) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, arg) = parse_eidx(ctx.clone())(i)?;
    return Ok((i, Expr::App { fun, arg }));
  }
}

/// The syntax for the ExprDecl::ExprLam command is:
/// ```text
/// <eidx'> #EL <info> <nidx> <eidx_1> <eidx_2>
/// ```
pub fn parse_expr_lam(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("L")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, info) = parse_bind(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, bod) = parse_eidx(ctx.clone())(i)?;
    return Ok((i, Expr::Lam { info, name, typ, bod }));
  }
}

/// The syntax for the ExprDecl::ExprLam command is:
/// ```text
/// <eidx'> #EL <info> <nidx> <eidx_1> <eidx_2>
/// ```
pub fn parse_expr_pi(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("P")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, info) = parse_bind(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, bod) = parse_eidx(ctx.clone())(i)?;
    return Ok((i, Expr::Pi { info, name, typ, bod }));
  }
}

/// The syntax for the ExprDecl::ExprLet command is:
/// ```text
/// <eidx'> #EZ <nidx> <eidx_1> <eidx_2> <eidx_3>
/// ```
pub fn parse_expr_let(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("Z")(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, name) = parse_nidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, typ) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, val) = parse_eidx(ctx.clone())(i)?;
    let (i, _) = parse_space1(i)?;
    let (i, bod) = parse_eidx(ctx.clone())(i)?;
    return Ok((i, Expr::Let { name, typ, val, bod }));
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

pub fn parse_nidx(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, NIdx, ParseError<Span>> {
  move |from: Span| {
    let (i, nidx) = map(parse_u64, NIdx)(from)?;
    let ctx = ctx.borrow();
    if let None = ctx.names.get(&nidx) {
      return Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::UndefinedNIdx(nidx),
      )));
    }
    else {
      return Ok((i, nidx));
    }
  }
}

pub fn parse_eidx(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, EIdx, ParseError<Span>> {
  move |from: Span| {
    let (i, eidx) = map(parse_u64, EIdx)(from)?;
    let ctx = ctx.borrow();
    if let None = ctx.exprs.get(&eidx) {
      return Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::UndefinedEIdx(eidx),
      )));
    }
    else {
      return Ok((i, eidx));
    }
  }
}

pub fn parse_uidx(
  ctx: RefCtx,
) -> impl Fn(Span) -> IResult<Span, UIdx, ParseError<Span>> {
  move |from: Span| {
    let (i, uidx) = map(parse_u64, UIdx)(from)?;
    let ctx = ctx.borrow();
    if let None = ctx.univs.get(&uidx) {
      return Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::UndefinedUIdx(uidx),
      )));
    }
    else {
      return Ok((i, uidx));
    }
  }
}

#[cfg(test)]
pub mod tests {
  use num_bigint::BigUint;
  use sp_std::collections::btree_map::BTreeMap;

  use super::*;

  #[test]
  fn test_parse_name() {
    fn test(i: &str) -> IResult<Span, Command, ParseError<Span>> {
      parse_name(Rc::new(RefCell::new(Ctx::new())))(Span::new(i))
    }
    let res = test("1 #NS 0 foo");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Name(NIdx(1), Name::Str {
        prev: NIdx(0),
        name: String::from("foo")
      })
    );
    let res = test("1 #NI 0 1234");
    println!("{:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Name(NIdx(1), Name::Int { prev: NIdx(0), int: 1234u64.into() })
    )
  }

  #[test]
  fn test_parse_univ() {
    fn test(i: &str) -> IResult<Span, Command, ParseError<Span>> {
      parse_univ(Rc::new(RefCell::new(Ctx::new())))(Span::new(i))
    }
    let res = test("1 #US 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Univ(UIdx(1), Univ::Succ { pred: UIdx(0) })
    );
    let res = test("1 #UM 0 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Univ(UIdx(1), Univ::Max { lhs: UIdx(0), rhs: UIdx(0) })
    );
    let res = test("1 #UIM 0 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Univ(UIdx(1), Univ::IMax { lhs: UIdx(0), rhs: UIdx(0) })
    );
    let res = test("1 #UP 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Univ(UIdx(1), Univ::Param { name: NIdx(0) })
    );
  }

  #[test]
  fn test_parse_expr() {
    fn test(i: &str) -> IResult<Span, Command, ParseError<Span>> {
      let mut ctx = Ctx::new();
      ctx
        .exprs
        .insert(EIdx(0), Expr::Const { name: NIdx(0), levels: vec![].into() });
      parse_expr(Rc::new(RefCell::new(ctx)))(Span::new(i))
    }
    let res = test("1 #EV 0");
    assert!(res.is_ok());
    assert_eq!(res.unwrap().1, Command::Expr(EIdx(1), Expr::Var { idx: 0 }));
    let res = test("1 #ES 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::Sort { univ: UIdx(0) })
    );
    let res = test("1 #EC 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::Const {
        name: NIdx(0),
        levels: Vector::new()
      })
    );
    let res = test("1 #EC 0 0 0 0");
    println!("{:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::Const {
        name: NIdx(0),
        levels: vec![UIdx(0), UIdx(0), UIdx(0)].into()
      })
    );
    let res = test("1 #EA 0 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::App { fun: EIdx(0), arg: EIdx(0) })
    );
    let res = test("1 #EL #BD 0 0 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::Lam {
        info: Bind::Default,
        name: NIdx(0),
        typ: EIdx(0),
        bod: EIdx(0)
      })
    );
    let res = test("1 #EP #BD 0 0 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::Pi {
        info: Bind::Default,
        name: NIdx(0),
        typ: EIdx(0),
        bod: EIdx(0)
      })
    );
    let res = test("1 #EZ 0 0 0 0");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Command::Expr(EIdx(1), Expr::Let {
        name: NIdx(0),
        typ: EIdx(0),
        val: EIdx(0),
        bod: EIdx(0)
      })
    );
  }

  #[test]
  fn test_parse_export() {
    fn test(i: &str) -> IResult<Span, Ctx, ParseError<Span>> {
      parse_export(Span::new(i))
    }
    #[rustfmt::skip]
    let res = test(
      "1 #NS 0 foo\n\
       2 #NS 1 bla\n\
       3 #NI 2 1\n\
       4 #NS 3 boo\n");
    assert!(res.is_ok());
    assert_eq!(res.unwrap().1, Ctx {
      univs: vec![(UIdx(0), Univ::Zero)].into_iter().collect(),
      names: vec![
        (NIdx(0), Name::Anon),
        (NIdx(1), Name::Str { prev: NIdx(0), name: String::from("foo") }),
        (NIdx(2), Name::Str { prev: NIdx(1), name: String::from("bla") }),
        (NIdx(3), Name::Int { prev: NIdx(2), int: BigUint::from(1u64) }),
        (NIdx(4), Name::Str { prev: NIdx(3), name: String::from("boo") }),
      ]
      .into_iter()
      .collect(),
      exprs: BTreeMap::new(),
      notations: Vec::new(),
      decls: Vec::new(),
    });
    #[rustfmt::skip]
    let res = test(
      "1 #NS 0 l1\n\
       2 #NS 0 l2\n\
       1 #US 0\n\
       2 #US 1\n\
       3 #UP 1\n\
       4 #UP 2\n\
       5 #UM 2 3\n\
       6 #UIM 5 4\n",
    );
    assert!(res.is_ok());
    assert_eq!(res.unwrap().1, Ctx {
      univs: vec![
        (UIdx(0), Univ::Zero),
        (UIdx(1), Univ::Succ { pred: UIdx(0) }),
        (UIdx(2), Univ::Succ { pred: UIdx(1) }),
        (UIdx(3), Univ::Param { name: NIdx(1) }),
        (UIdx(4), Univ::Param { name: NIdx(2) }),
        (UIdx(5), Univ::Max { lhs: UIdx(2), rhs: UIdx(3) }),
        (UIdx(6), Univ::IMax { lhs: UIdx(5), rhs: UIdx(4) }),
      ]
      .into_iter()
      .collect(),
      names: vec![
        (NIdx(0), Name::Anon),
        (NIdx(1), Name::Str { prev: NIdx(0), name: String::from("l1") }),
        (NIdx(2), Name::Str { prev: NIdx(0), name: String::from("l2") }),
      ]
      .into_iter()
      .collect(),
      exprs: BTreeMap::new(),
      notations: Vec::new(),
      decls: Vec::new(),
    });
  }
}

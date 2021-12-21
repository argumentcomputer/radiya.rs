use crate::{
  content::cid::ConstCid,
  name::Name,
  universe::Universe,
};
use sp_std::{
  boxed::Box,
  mem::MaybeUninit,
  ptr::NonNull,
  vec::Vec,
};

use num_bigint::BigUint;
use sp_im::vector::Vector;

use crate::parse::position::Pos;

use alloc::string::String;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
  Nat(BigUint),
  Str(String),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinderInfo {
  Default,
  Implicit,
  StrictImplicit,
  InstImplicit,
  AuxDecl,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
  BVar(Pos, usize),
  FVar(Pos, usize, Name, BinderInfo, Box<Expression>),
  Sort(Pos, Box<Universe>),
  Const(Pos, Name, Vector<Universe>),
  App(Pos, Box<Expression>, Box<Expression>),
  Lam(Pos, Name, BinderInfo, Box<Expression>, Box<Expression>),
  Pi(Pos, Name, BinderInfo, Box<Expression>, Box<Expression>),
  Let(Pos, Name, Box<Expression>, Box<Expression>, Box<Expression>),
  Lit(Pos, Literal),
  Fix(Pos, Name, Box<Expression>),
}

#[derive(Debug)]
pub enum GenExpression {
  BVar(Pos, usize),
  FVar(Pos, usize, Name, BinderInfo, NonNull<MaybeUninit<GenExpression>>),
  Sort(Pos, Box<Universe>),
  Const(Pos, Name, Vector<Universe>),
  App(
    Pos,
    NonNull<MaybeUninit<GenExpression>>,
    NonNull<MaybeUninit<GenExpression>>,
  ),
  Lam(
    Pos,
    Name,
    BinderInfo,
    NonNull<MaybeUninit<GenExpression>>,
    NonNull<MaybeUninit<GenExpression>>,
  ),
  Pi(
    Pos,
    Name,
    BinderInfo,
    NonNull<MaybeUninit<GenExpression>>,
    NonNull<MaybeUninit<GenExpression>>,
  ),
  Let(
    Pos,
    Name,
    NonNull<MaybeUninit<GenExpression>>,
    NonNull<MaybeUninit<GenExpression>>,
    NonNull<MaybeUninit<GenExpression>>,
  ),
  Lit(Pos, Literal),
  Fix(Pos, Name, NonNull<MaybeUninit<GenExpression>>),
}

#[cfg(test)]
pub mod tests {
  use crate::tests::{
    arbitrary_big_uint,
    frequency,
    gen_range,
    next_case,
  };
  use sp_std::mem;

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  impl Arbitrary for Literal {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Literal>)> = vec![
        (1, Box::new(|g| Literal::Nat(arbitrary_big_uint()(g)))),
        (1, Box::new(|g| Literal::Str(Arbitrary::arbitrary(g)))),
      ];
      frequency(g, input)
    }
  }

  impl Arbitrary for BinderInfo {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> BinderInfo>)> = vec![
        (1, Box::new(|_| BinderInfo::Default)),
        (1, Box::new(|_| BinderInfo::Implicit)),
        (1, Box::new(|_| BinderInfo::StrictImplicit)),
        (1, Box::new(|_| BinderInfo::InstImplicit)),
        (1, Box::new(|_| BinderInfo::AuxDecl)),
      ];
      frequency(g, input)
    }
  }
  #[derive(Debug, Clone, Copy)]
  pub enum ExpressionCase {
    BVAR,
    SORT,
    CONST,
    APP,
    LAM,
    PI,
    LET,
    LIT,
    FIX,
  }
  #[inline]
  pub fn alloc_val<T>(val: T) -> NonNull<T> {
    NonNull::new(Box::leak(Box::new(val))).unwrap()
  }
  pub fn arbitrary_term(
    g: &mut Gen,
    // defs: Defs,
    // uni_ctx: Vector<Name>,
    var_ctx0: Vector<Name>,
  ) -> Expression {
    let res = alloc_val(MaybeUninit::<GenExpression>::uninit());
    let mut stack = vec![(var_ctx0, res.clone())];
    let _term = Box::new(res);
    while let Some((ctx, mut ptr)) = stack.pop() {
      let depth = ctx.len();
      let gens: Vec<(usize, ExpressionCase)> = vec![
        (if ctx.len() == 0 { 0 } else { 100 }, ExpressionCase::BVAR),
        (100, ExpressionCase::SORT),
        //(100, ExpressionCase::CONST),
        (90usize.saturating_sub(depth), ExpressionCase::LAM),
        (80usize.saturating_sub(2 * depth), ExpressionCase::APP),
        (80usize.saturating_sub(2 * depth), ExpressionCase::PI),
        (30usize.saturating_sub(3 * depth), ExpressionCase::LET),
        (100, ExpressionCase::LIT),
      ];
      use GenExpression::*;
      match next_case(g, &gens) {
        ExpressionCase::LIT => unsafe {
          *ptr.as_mut() =
            MaybeUninit::new(Lit(Pos::None, Arbitrary::arbitrary(g)));
        },
        ExpressionCase::SORT => unsafe {
          *ptr.as_mut() =
            MaybeUninit::new(Sort(Pos::None, Arbitrary::arbitrary(g)));
        },
        ExpressionCase::CONST => unsafe {
          let nam: Name = Arbitrary::arbitrary(g);
          let mut us = Vec::new();
          let num = gen_range(g, 0..6);
          for _ in 0..num {
            us.push(Arbitrary::arbitrary(g));
          }
          *ptr.as_mut() = MaybeUninit::new(Const(Pos::None, nam, us.into()))
        },
        ExpressionCase::BVAR => {
          let gen = gen_range(g, 0..ctx.len());
          let n = &ctx[gen];
          let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(BVar(Pos::None, i as usize));
          }
        }
        ExpressionCase::LAM => {
          let nam: Name = Arbitrary::arbitrary(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(nam.clone());
          let info = Arbitrary::arbitrary(g);
          let typ = alloc_val(MaybeUninit::<GenExpression>::uninit());
          let bod = alloc_val(MaybeUninit::<GenExpression>::uninit());
          stack.push((ctx, typ));
          stack.push((ctx2, bod));
          unsafe {
            *ptr.as_mut() =
              MaybeUninit::new(Lam(Pos::None, nam.clone(), info, typ, bod));
          }
        }
        ExpressionCase::FIX => {
          let nam: Name = Arbitrary::arbitrary(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(nam.clone());
          let bod = alloc_val(MaybeUninit::<GenExpression>::uninit());
          stack.push((ctx2, bod));
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(Fix(Pos::None, nam.clone(), bod));
          }
        }
        ExpressionCase::APP => {
          let fun = alloc_val(MaybeUninit::<GenExpression>::uninit());
          let arg = alloc_val(MaybeUninit::<GenExpression>::uninit());
          stack.push((ctx.clone(), fun));
          stack.push((ctx, arg));
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(App(Pos::None, fun, arg));
          }
        }
        ExpressionCase::PI => {
          let nam: Name = Arbitrary::arbitrary(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(nam.clone());
          let info = Arbitrary::arbitrary(g);
          let typ = alloc_val(MaybeUninit::<GenExpression>::uninit());
          let bod = alloc_val(MaybeUninit::<GenExpression>::uninit());
          stack.push((ctx, typ));
          stack.push((ctx2, bod));
          unsafe {
            *ptr.as_mut() =
              MaybeUninit::new(Pi(Pos::None, nam.clone(), info, typ, bod));
          }
        }
        ExpressionCase::LET => {
          let nam: Name = Arbitrary::arbitrary(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(nam.clone());
          let typ = alloc_val(MaybeUninit::<GenExpression>::uninit());
          let exp = alloc_val(MaybeUninit::<GenExpression>::uninit());
          let bod = alloc_val(MaybeUninit::<GenExpression>::uninit());
          stack.push((ctx.clone(), typ));
          stack.push((ctx2.clone(), exp));
          stack.push((ctx2.clone(), bod));
          unsafe {
            *ptr.as_mut() =
              MaybeUninit::new(Let(Pos::None, nam.clone(), typ, exp, bod));
          }
        }
      }
    }
    unsafe {
      let term = Box::from_raw(res.as_ptr());
      let term = term.assume_init();
      mem::transmute::<GenExpression, Expression>(Box::into_inner(term))
    }
  }
}

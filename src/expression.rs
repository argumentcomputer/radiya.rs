use crate::{
  kvmap::KVMap,
  name,
  name::Name,
  universe::Univ,
};

use num_bigint::BigUint;
use sp_im::vector::Vector;

use alloc::string::String;
use sp_std::rc::Rc;

#[derive(Clone, Debug)]
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
  Rec,
}

#[derive(Clone, Debug)]
pub enum Expr {
  BVar(usize),
  FVar(Name),
  MVar(Name),
  Sort(Rc<Univ>),
  Const(Name, Vector<Rc<Univ>>),
  App(Rc<Expr>, Rc<Expr>),
  Lam(Name, BinderInfo, Rc<Expr>, Rc<Expr>),
  Pi(Name, BinderInfo, Rc<Expr>, Rc<Expr>),
  Let(Name, Rc<Expr>, Rc<Expr>, Rc<Expr>),
  Lit(Literal),
  MData(KVMap, Rc<Expr>),
  Proj(Name, BigUint, Rc<Expr>),
}

/// Short hand for creating a named sort
#[macro_export]
macro_rules! sort {
  ( $ctx:expr ) => {
    Expr::Sort(Univ::Param(name!($ctx)))
  };
}

/// Short hand for App node
#[macro_export]
macro_rules! app {
  ( $op:expr, $arg:expr ) => {
    Expr::App(Rc::new($op), Rc::new($arg))
  };
}

#[cfg(test)]
pub mod tests {
  use crate::tests::frequency;

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for BinderInfo {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> BinderInfo>)> = vec![
        (1, Box::new(|_| BinderInfo::Default)),
        (1, Box::new(|_| BinderInfo::Implicit)),
        (1, Box::new(|_| BinderInfo::StrictImplicit)),
        (1, Box::new(|_| BinderInfo::InstImplicit)),
        (1, Box::new(|_| BinderInfo::Rec)),
      ];
      frequency(g, input)
    }
  }
}

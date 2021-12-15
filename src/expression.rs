use crate::{
  name::Name,
  universe::Univ,
};

use num_bigint::BigUint;
use sp_im::vector::Vector;

use alloc::string::String;
use sp_std::rc::Rc;

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
pub enum Expr {
  BVar(usize),
  FVar(usize, Name, BinderInfo, Rc<Expr>),
  Sort(Rc<Univ>),
  Const(Name, Vector<Univ>),
  App(Rc<Expr>, Rc<Expr>),
  Lam(Name, BinderInfo, Rc<Expr>, Rc<Expr>),
  Pi(Name, BinderInfo, Rc<Expr>, Rc<Expr>),
  Let(Name, Rc<Expr>, Rc<Expr>, Rc<Expr>),
  Lit(Literal),
  Proj(Name, BigUint, Rc<Expr>),
}

#[cfg(test)]
pub mod tests {
  use crate::content::tests::frequency;

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
        (1, Box::new(|_| BinderInfo::AuxDecl)),
      ];
      frequency(g, input)
    }
  }
}

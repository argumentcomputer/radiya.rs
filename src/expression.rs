use crate::{
  kvmap::KVMap,
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
  StrictImplict,
  InstImplict,
  Rec,
}

#[derive(Clone, Debug)]
pub struct MData {}

#[derive(Clone, Debug)]
pub enum Expr {
  BVar(usize),
  FVar(usize),
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

//#[cfg(test)]
// pub mod tests {
//  use crate::content::tests::frequency;
//
//  use super::*;
//  use quickcheck::{
//    Arbitrary,
//    Gen,
//  };
//
//  impl Arbitrary for Bind {
//    fn arbitrary(g: &mut Gen) -> Self {
//      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Bind>)> = vec![
//        (1, Box::new(|_| Bind::Default)),
//        (1, Box::new(|_| Bind::Implicit)),
//        (1, Box::new(|_| Bind::Strict)),
//        (1, Box::new(|_| Bind::Class)),
//      ];
//      frequency(g, input)
//    }
//  }
//}

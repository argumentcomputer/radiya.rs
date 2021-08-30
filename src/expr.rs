use crate::{
  level::Level,
  name::Name,
};

use sp_im::vector::Vector;

use sp_std::boxed::Box;

#[derive(Clone, Debug)]
pub enum Bind {
  Default,
  Implicit,
  Strict,
  Class,
}

#[derive(Clone, Debug)]
pub enum Expr {
  Var(u64),
  Lam(Name, Bind, Box<Expr>, Box<Expr>),
  All(Name, Bind, Box<Expr>, Box<Expr>),
  Sort(Level),
  App(Box<Expr>, Box<Expr>),
  Let(Name, Bind, Box<Expr>, Box<Expr>, Box<Expr>),
  Const(Name, Vector<Level>),
  Local(Name, Bind, Box<Expr>),
}

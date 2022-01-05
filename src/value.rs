use crate::{
  expression::*,
  name::Name,
  universe::{
    Universe,
  },
};
use sp_std::{
  boxed::Box,
  rc::Rc,
  vec::Vec,
};
use sp_im::vector::Vector;

pub type ValuePtr = u32;
pub type Env = Vector<ValuePtr>;
pub type Args = Vec<ValuePtr>;
pub type Heap = Vec<Value>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
  Sort(Box<Universe>),
  App(Box<(Neutral, Args)>),
  Lam(Box<(Name, BinderInfo, Rc<Expression>, Env)>),
  Pi(Box<(Name, BinderInfo, Rc<Expression>, Rc<Expression>, Env)>),
  Lit(Box<Literal>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Neutral {
  Var(usize),
  Const(Name, Vector<Universe>),
}

#[inline(always)]
pub fn lam(nam: Name, bnd: BinderInfo, bod: Rc<Expression>, env: Env, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::Lam(Box::new((nam, bnd, bod, env))));
  (heap.len()-1) as ValuePtr
}

#[inline(always)]
pub fn app(neu: Neutral, args: Args, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::App(Box::new((neu, args))));
  (heap.len()-1) as ValuePtr
}

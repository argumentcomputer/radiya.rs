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
  // Used for recursive definitions
  Thunk(Box<(Rc<Expression>, Env)>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Neutral {
  Var(usize),
  Const(Name, Vector<Universe>),
}

#[inline(always)]
pub fn push_lam(nam: Name, bnd: BinderInfo, bod: Rc<Expression>, env: Env, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::Lam(Box::new((nam, bnd, bod, env))));
  (heap.len()-1) as ValuePtr
}

#[inline(always)]
pub fn push_app(neu: Neutral, args: Args, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::App(Box::new((neu, args))));
  (heap.len()-1) as ValuePtr
}

#[inline(always)]
pub fn push_pi(nam: Name, bnd: BinderInfo, dom: Rc<Expression>, img: Rc<Expression>, env: Env, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::Pi(Box::new((nam, bnd, dom, img, env))));
  (heap.len()-1) as ValuePtr
}

#[inline(always)]
pub fn push_lit(lit: Literal, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::Lit(Box::new(lit)));
  (heap.len()-1) as ValuePtr
}


#[inline(always)]
pub fn push_sort(univ: Box<Universe>, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::Sort(univ));
  (heap.len()-1) as ValuePtr
}

#[inline(always)]
pub fn push_thunk(bod: Rc<Expression>, env: Env, heap: &mut Heap) -> ValuePtr {
  heap.push(Value::Thunk(Box::new((bod, env))));
  (heap.len()-1) as ValuePtr
}

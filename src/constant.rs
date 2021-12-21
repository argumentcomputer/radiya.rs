use crate::{
  expression::Expression,
  name::Name,
  parse::position::Pos,
};
use num_bigint::BigUint;
use sp_im::Vector;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum QuotKind {
  Type,
  Ctor,
  Lift,
  Ind,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DefinitionSafety {
  Unsafe,
  Safe,
  Partial,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecursorRule {
  pub ctor: Name,
  pub num_fields: usize,
  pub rhs: Expression,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Intro {
  ctor: Name,
  typ: Expression,
}

pub enum Constant {
  Quotient {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    kind: QuotKind,
  },
  Axiom {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    is_unsafe: bool,
  },
  Theorem {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    val: Expression,
  },
  Opaque {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    val: Expression,
    is_unsafe: bool,
  },
  Definition {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    val: Expression,
    safety: DefinitionSafety,
  },
  Inductive {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    ctors: Vector<Intro>,
    params: usize,
    indices: usize,
    is_unsafe: bool,
  },
  Constructor {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    induct: Name,
    ctor_idx: usize,
    params: usize,
    fields: usize,
    is_unsafe: bool,
  },
  Recursor {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    params: usize,
    indices: usize,
    motives: usize,
    minors: usize,
    rules: Vector<RecursorRule>,
    k: bool,
    is_unsafe: bool,
  },
}

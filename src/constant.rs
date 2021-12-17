use crate::{
  expression::Expr,
  name::Name,
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
  pub num_fields: BigUint,
  pub rhs: Expr,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Intro {
  ctor: Name,
  typ: Expr,
}

pub enum Constant {
  Quot {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    kind: QuotKind,
  },
  Axiom {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    is_unsafe: bool,
  },
  Theorem {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    val: Expr,
  },
  Opaque {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    val: Expr,
    is_unsafe: bool,
  },
  Definition {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    val: Expr,
    safety: DefinitionSafety,
  },
  Inductive {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    ctors: Vector<(Name, Expr)>,
    num_params: BigUint,
    num_indices: BigUint,
    all: Vector<Name>,
    is_rec: bool,
    is_unsafe: bool,
    is_reflexive: bool,
  },
  Constructor {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    induct: Name,
    ctor_idx: BigUint,
    num_params: BigUint,
    num_fields: BigUint,
    is_unsafe: bool,
  },
  Recursor {
    name: Name,
    level_params: Vector<Name>,
    typ: Expr,
    num_params: BigUint,
    num_indices: BigUint,
    num_motives: BigUint,
    num_minors: BigUint,
    ruls: Vector<RecursorRule>,
    k: bool,
    is_unsafe: bool,
  },
}

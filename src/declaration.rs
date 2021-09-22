use crate::{
  expression::Expr,
  name::Name,
};
use num_bigint::BigUint;
use sp_im::Vector;

#[derive(Clone, Debug)]
pub enum ReducibilityHints {
  Opaque,
  Abbrev,
  Regular(u32),
}

pub enum Declaration {
  Axiom(AxiomVal),
  Definition(DefinitionVal),
  Theorem(TheoremVal),
  Opaque(OpaqueVal),
  Quot,
  MutualDefn(Vector<DefinitionVal>),
  Inductive {
    lparams: Vector<Name>,
    nparams: BigUint,
    types: Vector<InductiveType>,
    is_unsafe: bool,
  },
}

pub struct ConstantVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
}

pub struct AxiomVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub is_unsafe: bool,
}

pub enum DefinitionSafety {
  Unsafe,
  Safe,
  Partial,
}

pub struct DefinitionVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub val: Expr,
  pub hints: ReducibilityHints,
  pub safety: DefinitionSafety,
}

pub struct TheoremVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub val: Expr,
}

pub struct OpaqueVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub val: Expr,
  pub is_unsafe: bool,
}

pub struct Constructor {
  pub name: Name,
  pub typ_: Expr,
}

pub struct InductiveType {
  pub name: Name,
  pub typ: Expr,
  pub ctors: Vector<Constructor>,
}

pub struct InductiveVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub num_params: BigUint,
  pub num_indices: BigUint,
  pub all: Vector<Name>,
  pub is_rec: bool,
  pub is_unsafe: bool,
  pub is_reflexive: bool,
  pub is_nested: bool,
}

pub struct ConstructorVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub induct: Name,
  pub ctor_idx: BigUint,
  pub num_params: BigUint,
  pub num_fields: BigUint,
  pub is_unsafe: bool,
}

pub struct RecursorRule {
  pub ctor: Name,
  pub num_fields: BigUint,
  pub rhs: Expr,
}

pub struct RecursorVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub num_params: BigUint,
  pub num_indices: BigUint,
  pub num_motives: BigUint,
  pub num_minors: BigUint,
  pub ruls: Vector<RecursorRule>,
  pub k: bool,
  pub is_unsafe: bool,
}

pub enum QuotKind {
  Type,
  Ctor,
  Lift,
  Ind,
}

pub struct QuotVal {
  pub name: Name,
  pub level_params: Vector<Name>,
  pub typ: Expr,
  pub kind: QuotKind,
}

pub enum ConstantInfo {
  Axiom(AxiomVal),
  Definition(DefinitionVal),
  Theorem(TheoremVal),
  Opaque(OpaqueVal),
  Quot(QuotVal),
  Inductive(InductiveVal),
  Constructor(ConstructorVal),
  Recursor(RecursorVal),
}

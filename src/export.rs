use crate::expression::Bind;

use num_bigint::BigUint;
use sp_im::vector::Vector;

use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use alloc::string::String;

// universe index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct UIdx(pub u64);

// expr index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct EIdx(pub u64);

// name index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct NIdx(pub u64);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Ctx {
  pub univs: BTreeMap<UIdx, Univ>,
  pub names: BTreeMap<NIdx, Name>,
  pub exprs: BTreeMap<EIdx, Expr>,
  pub notations: Vec<Notation>,
  pub decls: Vec<Decl>,
}

impl Ctx {
  pub fn new() -> Self {
    Ctx {
      univs: vec![(UIdx(0), Univ::Zero)].into_iter().collect(),
      names: vec![(NIdx(0), Name::Anon)].into_iter().collect(),
      exprs: BTreeMap::new(),
      notations: Vec::new(),
      decls: Vec::new(),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Name {
  /// Anonymous name
  Anon,
  /// Extends a hierarchical name `prev` with `name` to make `prev.name`
  Str { prev: NIdx, name: String },
  /// Extends a hierarchical name `prev` with `int` to make `prev.int`
  Int { prev: NIdx, int: BigUint },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Univ {
  /// The 0 universe
  Zero,
  /// Defines the successor universe of pred
  Succ { pred: UIdx },
  /// Defines the maximum universe of the lhs and rhs:
  Max { lhs: UIdx, rhs: UIdx },
  /// Defines the impredicative maximum universe for lhs and
  /// rhs, which is zero if rhs is zero and #UM otherwise.
  IMax { lhs: UIdx, rhs: UIdx },
  /// Defines a universe parameter
  Param { name: NIdx },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  /// #EV
  Var { idx: u64 },
  /// #ES
  Sort { univ: UIdx },
  /// #EC
  Const { name: NIdx, levels: Vector<UIdx> },
  /// #EA
  App { fun: EIdx, arg: EIdx },
  /// #EL
  Lam { info: Bind, name: NIdx, typ: EIdx, bod: EIdx },
  /// #EP
  Pi { info: Bind, name: NIdx, typ: EIdx, bod: EIdx },
  /// #EZ
  Let { name: NIdx, typ: EIdx, val: EIdx, bod: EIdx },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
  /// #DEF
  Definition { name: NIdx, typ: EIdx, val: EIdx, levels: Vector<NIdx> },
  /// #IND
  Inductive {
    num_params: u64,
    name: NIdx,
    typ: EIdx,
    intros: Vector<(NIdx, EIdx)>,
    levels: Vector<NIdx>,
  },
  /// #AX
  Axiom { name: NIdx, typ: EIdx, levels: Vector<NIdx> },
  /// #QUOT
  Quotient,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Notation {
  Prefix { name: NIdx, prec: u64, token: String },
  Infix { name: NIdx, prec: u64, token: String },
  Postfix { name: NIdx, prec: u64, token: String },
}

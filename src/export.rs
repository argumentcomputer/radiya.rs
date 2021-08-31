use crate::expr::Bind;

use num_bigint::BigUint;
use sp_im::vector::Vector;

use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use alloc::string::String;

// universe index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Uid(pub u64);

// expr index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Eid(pub u64);

// name index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Nid(pub u64);

#[derive(Clone, Debug)]
pub struct Ctx {
  pub univs: BTreeMap<Uid, UnivDecl>,
  pub names: BTreeMap<Nid, NameDecl>,
  pub exprs: BTreeMap<Eid, ExprDecl>,
  pub notations: Vec<Notation>,
  pub decls: Vec<TopDecl>,
}

impl Ctx {
  pub fn new() -> Self {
    Ctx {
      univs: BTreeMap::new(),
      names: BTreeMap::new(),
      exprs: BTreeMap::new(),
      notations: Vec::new(),
      decls: Vec::new(),
    }
  }
}

#[derive(Clone, Debug)]
pub enum NameDecl {
  /// Extends a hierarchical name `prev` with `name` to make `prev.name`
  NameStr { prev: Nid, name: String },
  /// Extends a hierarchical name `prev` with `int` to make `prev.int`
  NameInt { prev: Nid, int: BigUint },
}

#[derive(Clone, Debug)]
pub enum UnivDecl {
  /// Defines the successor universe of pred
  UnivSucc { pred: Uid },
  /// Defines the maximum universe of the lhs and rhs:
  UnivMax { lhs: Uid, rhs: Uid },
  /// Defines the impredicative maximum universe for lhs and
  /// rhs, which is zero if rhs is zero and #UM otherwise.
  UnivIMax { lhs: Uid, rhs: Uid },
  /// Defines a universe parameter
  UnivParam { name: Nid },
}

#[derive(Clone, Debug)]
pub enum ExprDecl {
  /// #EV
  ExprVar { idx: u64 },
  /// #ES
  ExprSort { univ: Uid },
  /// #EC
  ExprConst { name: Nid, levels: Vector<Uid> },
  /// #EA
  ExprApp { fun: Eid, arg: Eid },
  /// #EL
  ExprLam { info: Bind, name: Nid, typ: Eid, bod: Eid },
  /// #EP
  ExprPi { info: Bind, name: Nid, typ: Eid, bod: Eid },
  /// #EZ
  ExprLet { name: Nid, typ: Eid, val: Eid, bod: Eid },
}

#[derive(Clone, Debug)]
pub enum Notation {
  Prefix { name: Nid, prec: u64, token: String },
  Infix { name: Nid, prec: u64, token: String },
  Postfix { name: Nid, prec: u64, token: String },
}

#[derive(Clone, Debug)]
pub enum TopDecl {
  /// #DEF
  Definition { name: Nid, typ: Eid, val: Eid, levels: Vector<Nid> },
  /// #IND
  Inductive {
    num_params: u64,
    name: Nid,
    typ: Eid,
    intros: Vector<(Nid, Eid)>,
    levels: Vector<Nid>,
  },
  /// #AX
  Axiom { name: Nid, typ: Eid, levels: Vector<Nid> },
  /// #QUOT
  Quotient,
}

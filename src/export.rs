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
  pub decls: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub enum UnivDecl {
  /// #US
  UnivSucc { pred: Uid },
  /// #UM
  UnivMax { lhs: Uid, rhs: Uid },
  /// #UI
  UnivIMax { lhs: Uid, rhs: Uid },
  /// #UP
  UnivParam { name: Nid },
}

#[derive(Clone, Debug)]
pub enum NameDecl {
  /// #NS
  NameStr { prev: Nid, name: String },
  /// #NI
  NameInt { prev: Nid, name: BigUint },
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
  ExprAll { info: Bind, name: Nid, typ: Eid, bod: Eid },
  /// #EZ
  ExprLet { info: Bind, name: Nid, typ: Eid, val: Eid, bod: Eid },
}

#[derive(Clone, Debug)]
pub enum Notation {
  Prefix { name: Nid, prec: u64, token: String },
  Infix { name: Nid, prec: u64, token: String },
  Postfix { name: Nid, prec: u64, token: String },
}

#[derive(Clone, Debug)]
pub enum Decl {
  /// #DEF
  Definition { name: Nid, typ: Eid, val: Eid, levels: Vector<Nid> },
  /// #IND
  Inductive {
    num: u64,
    name: Nid,
    typ: Eid,
    intros: Vector<Eid>,
    levels: Vector<Nid>,
  },
  /// #AX
  Axiom { name: Nid, exp: Eid, levels: Vector<Nid> },
  /// #QUOT
  Quot,
}

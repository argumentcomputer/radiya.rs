pub mod bind;
pub mod cid;
pub mod expr;
pub mod ipld_error;
pub mod name;
pub mod univ;

pub use cid::{
  DeclCid,
  ExprCid,
  NameCid,
  UnivCid,
};
pub use expr::Expr;
pub use name::Name;
pub use univ::Univ;

use crate::{
  export,
  expression::Bind,
};

use alloc::{
  borrow::ToOwned,
  string::String,
};
use num_bigint::BigUint;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
  Ipld,
};
use sp_multihash::{
  Code,
  MultihashDigest,
};
use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};

//#[derive(Clone, PartialEq, Eq, Debug)]
// pub struct Ctx {
//  pub univs: BTreeMap<UnivCid, Univ>,
//  pub names: BTreeMap<NameCid, Name>,
//  pub exprs: BTreeMap<ExprCid, Expr>,
//  pub notations: Vec<Notation>,
//  pub decls: BTreeMap<DeclCid, Decl>,
//}
//
//#[derive(Clone, Debug, PartialEq, Eq)]
// pub enum Decl {
//  Definition {
//    name: NameCid,
//    typ: ExprCid,
//    val: ExprCid,
//    levels: Vector<NameCid>,
//  },
//  Inductive {
//    num_params: u64,
//    name: NameCid,
//    typ: ExprCid,
//    intros: Vector<(NameCid, ExprCid)>,
//    levels: Vector<NameCid>,
//  },
//  Axiom {
//    name: NameCid,
//    typ: ExprCid,
//    levels: Vector<NameCid>,
//  },
//  Quotient,
//}
//
//#[derive(Clone, Debug, PartialEq, Eq)]
// pub enum Notation {
//  Prefix { name: NameCid, prec: u64, token: String },
//  Infix { name: NameCid, prec: u64, token: String },
//  Postfix { name: NameCid, prec: u64, token: String },
//}

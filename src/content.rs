pub mod cid;
pub mod constant;
// pub mod environment;
pub mod expr;
pub mod ipld;
pub mod name;
pub mod univ;

pub use cid::{
  ConstCid,
  ExprCid,
  NameCid,
  UnivCid,
};
// pub use constant::Const;
// pub use expr::Expr;
// pub use name::Name;
// pub use univ::Univ;
//
// use alloc::{
//  borrow::ToOwned,
//  string::String,
//};
// use num_bigint::BigUint;
// use sp_ipld::{
//  dag_cbor::DagCborCodec,
//  Codec,
//  Ipld,
//};
// use sp_multihash::{
//  Code,
//  MultihashDigest,
//};
// use sp_std::{
//  collections::btree_map::BTreeMap,
//  vec::Vec,
//};
//
//#[derive(Clone, PartialEq, Eq, Debug)]
// pub struct Ctx {
//  pub univs: BTreeMap<UnivCid, Univ>,
//  pub names: BTreeMap<NameCid, Name>,
//  pub exprs: BTreeMap<ExprCid, Expr>,
//  pub consts: BTreeMap<ConstCid, Const>,
//}
//
#[cfg(test)]
pub mod tests {
  use quickcheck::Gen;
  use rand::Rng;

  pub fn frequency<T, F: Fn(&mut Gen) -> T>(
    g: &mut Gen,
    gens: sp_std::vec::Vec<(i64, F)>,
  ) -> T {
    if gens.iter().any(|(v, _)| *v < 0) {
      panic!("Negative weight");
    }
    let sum: i64 = gens.iter().map(|x| x.0).sum();
    let mut rng = rand::thread_rng();
    let mut weight: i64 = rng.gen_range(1..=sum);
    // let mut weight: i64 = g.rng.gen_range(1, sum);
    for gen in gens {
      if weight - gen.0 <= 0 {
        return gen.1(g);
      }
      else {
        weight -= gen.0;
      }
    }
    panic!("Calculation error for weight = {}", weight);
  }
}

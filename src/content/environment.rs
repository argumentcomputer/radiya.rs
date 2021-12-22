use sp_std::collections::btree_map::BTreeMap;

use sp_ipld::Ipld;

use crate::{
  content::{
    cid::{
      ConstCid,
      ConstMetaCid,
      ExprCid,
      NameCid,
      UnivCid,
    },
    ipld::{
      IpldEmbed,
      IpldError,
    },
    univ::Univ,
  },
  expression::Literal,
  name::Name,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Env {
  pub constants: BTreeMap<NameCid, (ConstCid, ConstMetaCid)>,
}

impl Env {
  pub fn new() -> Self { Env { constants: BTreeMap::new() } }
}

impl IpldEmbed for Env {
  fn to_ipld(&self) -> Ipld { todo!() }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> { todo!() }
}

use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use sp_ipld::Ipld;

use crate::{
  content::{
    cid::{
      ConstCid,
      ExprCid,
      LiteralCid,
      MetaCid,
      NameCid,
      UnivCid,
    },
    constant::Const,
    expr::Expr,
    ipld::{
      IpldEmbed,
      IpldError,
    },
    metadata::Metadata,
    univ::Univ,
  },
  expression::Literal,
  name::Name,
};

pub struct Cache {
  pub metadata: BTreeMap<MetaCid, Metadata>,
  pub names: BTreeMap<NameCid, Name>,
  pub universes: BTreeMap<UnivCid, Univ>,
  pub expressions: BTreeMap<ExprCid, Expr>,
  pub literals: BTreeMap<LiteralCid, Literal>,
  pub constants: BTreeMap<ConstCid, Const>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Environment {
  pub constants: BTreeMap<Name, (MetaCid, ConstCid)>,
}
impl IpldEmbed for Environment {
  fn to_ipld(&self) -> Ipld { todo!() }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> { todo!() }
}

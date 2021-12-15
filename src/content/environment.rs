use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use crate::{
  content::cid::{
    ConstCid,
    MetaCid,
  },
  name::Name,
};

pub struct Environment {
  pub constants: BTreeMap<Name, ConstCid>,
  pub metadata: BTreeMap<ConstCid, MetaCid>,
}

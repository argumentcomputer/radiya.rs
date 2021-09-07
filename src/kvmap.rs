use crate::name::Name;
use alloc::string::String;
use num_bigint::BigUint;
use sp_std::collections::btree_map::BTreeMap;

#[derive(Clone, Debug)]
pub enum DataValue {
  Str(String),
  Bool(bool),
  Name(Name),
  Nat(BigUint),
  // Int(BigInt),
}

pub type KVMap = BTreeMap<Name, DataValue>;

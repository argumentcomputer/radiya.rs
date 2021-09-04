use crate::content::{
  cid::NameCid,
  ipld_error::IpldError,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Name {
  Anon,
  Str { prev: NameCid, name: String },
  Int { prev: NameCid, int: BigUint },
}

impl Name {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Anon => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(0)]),
      Self::Str { prev, name } => Ipld::List(vec![
        Ipld::Integer(0),
        Ipld::Integer(1),
        Ipld::Link(prev.0),
        Ipld::String(name.clone()),
      ]),
      Self::Int { prev, int } => Ipld::List(vec![
        Ipld::Integer(0),
        Ipld::Integer(2),
        Ipld::Link(prev.0),
        Ipld::Bytes(int.to_bytes_be()),
      ]),
    }
  }

  pub fn cid(&self) -> NameCid {
    NameCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), Ipld::Integer(0)] => Ok(Name::Anon),
        [Ipld::Integer(0), Ipld::Integer(1), Ipld::Link(x), Ipld::String(y)] => {
          if let Some(prev) = NameCid::from_cid(*x) {
            Ok(Name::Str { prev, name: y.clone() })
          }
          else {
            Err(IpldError::NameCid(*x))
          }
        }
        [Ipld::Integer(0), Ipld::Integer(2), Ipld::Link(x), Ipld::Bytes(y)] => {
          if let Some(prev) = NameCid::from_cid(*x) {
            Ok(Name::Int { prev, int: BigUint::from_bytes_be(&y) })
          }
          else {
            Err(IpldError::NameCid(*x))
          }
        }

        xs => Err(IpldError::Name(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Name(xs.to_owned())),
    }
  }
}

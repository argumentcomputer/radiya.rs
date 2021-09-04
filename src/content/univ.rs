use crate::content::{
  cid::{
    NameCid,
    UnivCid,
  },
  ipld_error::IpldError,
};
use alloc::borrow::ToOwned;

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
pub enum Univ {
  Zero,
  Succ { pred: UnivCid },
  Max { lhs: UnivCid, rhs: UnivCid },
  IMax { lhs: UnivCid, rhs: UnivCid },
  Param { name: NameCid },
}

impl Univ {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Zero => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(0)]),
      Self::Succ { pred } => {
        Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(1), Ipld::Link(pred.0)])
      }
      Self::Max { lhs, rhs } => Ipld::List(vec![
        Ipld::Integer(1),
        Ipld::Integer(2),
        Ipld::Link(lhs.0),
        Ipld::Link(rhs.0),
      ]),
      Self::IMax { lhs, rhs } => Ipld::List(vec![
        Ipld::Integer(1),
        Ipld::Integer(3),
        Ipld::Link(lhs.0),
        Ipld::Link(rhs.0),
      ]),
      Self::Param { name } => {
        Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(4), Ipld::Link(name.0)])
      }
    }
  }

  pub fn cid(&self) -> UnivCid {
    UnivCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(1), Ipld::Integer(0)] => Ok(Univ::Zero),
        [Ipld::Integer(1), Ipld::Integer(1), Ipld::Link(x)] => {
          if let Some(pred) = UnivCid::from_cid(*x) {
            Ok(Univ::Succ { pred })
          }
          else {
            Err(IpldError::UnivCid(*x))
          }
        }
        [Ipld::Integer(1), Ipld::Integer(2), Ipld::Link(x), Ipld::Link(y)] => {
          if let (Some(lhs), Some(rhs)) =
            (UnivCid::from_cid(*x), UnivCid::from_cid(*y))
          {
            Ok(Univ::Max { lhs, rhs })
          }
          else {
            Err(IpldError::UnivCid(*x))
          }
        }
        [Ipld::Integer(1), Ipld::Integer(3), Ipld::Link(x), Ipld::Link(y)] => {
          if let (Some(lhs), Some(rhs)) =
            (UnivCid::from_cid(*x), UnivCid::from_cid(*y))
          {
            Ok(Univ::IMax { lhs, rhs })
          }
          else {
            Err(IpldError::UnivCid(*x))
          }
        }
        [Ipld::Integer(1), Ipld::Integer(4), Ipld::Link(x)] => {
          if let Some(name) = NameCid::from_cid(*x) {
            Ok(Univ::Param { name })
          }
          else {
            Err(IpldError::NameCid(*x))
          }
        }
        xs => Err(IpldError::Univ(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Univ(xs.to_owned())),
    }
  }
}

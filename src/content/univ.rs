use crate::content::{
  cid::UnivCid,
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
pub enum Univ {
  Zero,
  Succ { pred: UnivCid },
  Max { lhs: UnivCid, rhs: UnivCid },
  IMax { lhs: UnivCid, rhs: UnivCid },
  Param { idx: BigUint },
}

impl Univ {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Zero => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(0)]),
      Self::Succ { pred } => {
        Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(1), Ipld::Link(pred.0)])
      }
      Self::Max { lhs, rhs } => Ipld::List(vec![
        Ipld::Integer(3),
        Ipld::Integer(2),
        Ipld::Link(lhs.0),
        Ipld::Link(rhs.0),
      ]),
      Self::IMax { lhs, rhs } => Ipld::List(vec![
        Ipld::Integer(3),
        Ipld::Integer(3),
        Ipld::Link(lhs.0),
        Ipld::Link(rhs.0),
      ]),
      Self::Param { idx } => Ipld::List(vec![
        Ipld::Integer(3),
        Ipld::Integer(4),
        Ipld::Bytes(idx.to_bytes_be()),
      ]),
    }
  }

  pub fn cid(&self) -> UnivCid {
    UnivCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(3), Integer(0)] => Ok(Univ::Zero),
        [Integer(3), Integer(1), Link(p)] => {
          let pred = UnivCid::from_cid(*p)?;
          Ok(Univ::Succ { pred })
        }
        [Integer(3), Integer(2), Link(l), Link(r)] => {
          let lhs = UnivCid::from_cid(*l)?;
          let rhs = UnivCid::from_cid(*r)?;
          Ok(Univ::Max { lhs, rhs })
        }
        [Integer(3), Integer(3), Link(l), Link(r)] => {
          let lhs = UnivCid::from_cid(*l)?;
          let rhs = UnivCid::from_cid(*r)?;
          Ok(Univ::IMax { lhs, rhs })
        }
        [Integer(3), Integer(4), Bytes(bs)] => {
          Ok(Univ::Param { idx: BigUint::from_bytes_be(bs) })
        }
        xs => Err(IpldError::Univ(List(xs.to_owned()))),
      },
      xs => Err(IpldError::Univ(xs.to_owned())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::{
    content::tests::frequency,
    tests::arbitrary_big_uint,
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for Univ {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Univ>)> = vec![
        (1, Box::new(|_| Univ::Zero)),
        (1, Box::new(|g| Univ::Succ { pred: Arbitrary::arbitrary(g) })),
        (
          1,
          Box::new(|g| Univ::Max {
            lhs: Arbitrary::arbitrary(g),
            rhs: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Univ::IMax {
            lhs: Arbitrary::arbitrary(g),
            rhs: Arbitrary::arbitrary(g),
          }),
        ),
        (1, Box::new(|g| Univ::Param { idx: arbitrary_big_uint()(g) })),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn univ_ipld(x: Univ) -> bool {
    match Univ::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

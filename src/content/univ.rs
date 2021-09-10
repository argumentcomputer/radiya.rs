use crate::content::ipld::{
  IpldEmbed,
  IpldError,
  NameCid,
  UnivCid,
  UNIV,
};
use alloc::{
  borrow::ToOwned,
  string::String,
};

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

impl IpldEmbed for Univ {
  const CODEC: u64 = UNIV;

  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Zero => Ipld::List(vec![Ipld::String(String::from("#UZ"))]),
      Self::Succ { pred } => Ipld::List(vec![
        Ipld::String(String::from("#US")),
        Ipld::Link(pred.to_dyn()),
      ]),
      Self::Max { lhs, rhs } => Ipld::List(vec![
        Ipld::String(String::from("#UM")),
        Ipld::Link(lhs.to_dyn()),
        Ipld::Link(rhs.to_dyn()),
      ]),
      Self::IMax { lhs, rhs } => Ipld::List(vec![
        Ipld::String(String::from("#UIM")),
        Ipld::Link(lhs.to_dyn()),
        Ipld::Link(rhs.to_dyn()),
      ]),
      Self::Param { name } => Ipld::List(vec![
        Ipld::String(String::from("#UP")),
        Ipld::Link(name.to_dyn()),
      ]),
    }
  }

  fn cid(&self) -> UnivCid {
    UnivCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag)] if tag == "#UZ" => Ok(Univ::Zero),
        [String(tag), Link(p)] if tag == "#US" => {
          let pred = UnivCid::from_dyn(p)?;
          Ok(Univ::Succ { pred })
        }
        [String(tag), Link(l), Link(r)] if tag == "#UM" => {
          let lhs = UnivCid::from_dyn(l)?;
          let rhs = UnivCid::from_dyn(r)?;
          Ok(Univ::Max { lhs, rhs })
        }
        [String(tag), Link(l), Link(r)] if tag == "#UIM" => {
          let lhs = UnivCid::from_dyn(l)?;
          let rhs = UnivCid::from_dyn(r)?;
          Ok(Univ::IMax { lhs, rhs })
        }
        [String(tag), Link(n)] if tag == "#UP" => {
          let name = NameCid::from_dyn(n)?;
          Ok(Univ::Param { name })
        }
        xs => Err(IpldError::ExpectedUniv(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedUniv(xs.to_owned())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::content::tests::frequency;

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
        (1, Box::new(|g| Univ::Param { name: Arbitrary::arbitrary(g) })),
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

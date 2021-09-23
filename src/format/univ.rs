use crate::format::{
  ipld_embed::{
    IpldEmbed,
    IpldError,
  },
  name::NameCid,
  UNIV_CODEC,
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

use sp_cid::{
  Cid,
  Version,
};
use sp_multihash::{
  Code,
  Multihash,
  MultihashDigest,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivCid {
  pub hash: Multihash,
}

impl IpldEmbed for UnivCid {
  fn cid(&self) -> Cid { Cid::new_v1(UNIV_CODEC, self.hash) }

  fn to_ipld(&self) -> Ipld { Ipld::Link(self.cid()) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid)
        if cid.codec() == UNIV_CODEC && cid.version() == Version::V1 =>
      {
        Ok(UnivCid { hash: *cid.hash() })
      }
      _ => Err(IpldError::ExpectedUnivCid(ipld.clone())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Univ {
  Zero,
  Succ { pred: UnivCid },
  Max { lhs: UnivCid, rhs: UnivCid },
  IMax { lhs: UnivCid, rhs: UnivCid },
  Param { name: NameCid },
}

impl IpldEmbed for Univ {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Zero => Ipld::List(vec![Ipld::String(String::from("#UZ"))]),
      Self::Succ { pred } => {
        Ipld::List(vec![Ipld::String(String::from("#US")), pred.to_ipld()])
      }
      Self::Max { lhs, rhs } => Ipld::List(vec![
        Ipld::String(String::from("#UM")),
        lhs.to_ipld(),
        rhs.to_ipld(),
      ]),
      Self::IMax { lhs, rhs } => Ipld::List(vec![
        Ipld::String(String::from("#UIM")),
        lhs.to_ipld(),
        rhs.to_ipld(),
      ]),
      Self::Param { name } => {
        Ipld::List(vec![Ipld::String(String::from("#UP")), name.to_ipld()])
      }
    }
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      UNIV_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag)] if tag == "#UZ" => Ok(Univ::Zero),
        [String(tag), p] if tag == "#US" => {
          let pred = UnivCid::from_ipld(p)?;
          Ok(Univ::Succ { pred })
        }
        [String(tag), l, r] if tag == "#UM" => {
          let lhs = UnivCid::from_ipld(l)?;
          let rhs = UnivCid::from_ipld(r)?;
          Ok(Univ::Max { lhs, rhs })
        }
        [String(tag), l, r] if tag == "#UIM" => {
          let lhs = UnivCid::from_ipld(l)?;
          let rhs = UnivCid::from_ipld(r)?;
          Ok(Univ::IMax { lhs, rhs })
        }
        [String(tag), n] if tag == "#UP" => {
          let name = NameCid::from_ipld(n)?;
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
  use crate::{
    format::tests::arbitrary_cid,
    tests::frequency,
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  impl Arbitrary for UnivCid {
    fn arbitrary(g: &mut Gen) -> Self {
      let cid = arbitrary_cid(g, UNIV_CODEC);
      UnivCid { hash: *cid.hash() }
    }
  }

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

use crate::format::{
  ipld_embed::{
    IpldEmbed,
    IpldError,
  },
  NAME_CODEC,
};

use alloc::{
  borrow::ToOwned,
  string::String,
};
use num_bigint::BigUint;

use sp_cid::{
  Cid,
  Version,
};

use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
  Ipld,
};

use sp_multihash::{
  Code,
  Multihash,
  MultihashDigest,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameCid {
  pub hash: Multihash,
}

impl IpldEmbed for NameCid {
  fn cid(&self) -> Cid { Cid::new_v1(NAME_CODEC, self.hash) }

  fn to_ipld(&self) -> Ipld { Ipld::Link(self.cid()) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid)
        if cid.codec() == NAME_CODEC && cid.version() == Version::V1 =>
      {
        Ok(NameCid { hash: *cid.hash() })
      }
      _ => Err(IpldError::ExpectedNameCid(ipld.clone())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Name {
  Anon,
  Str { prev: NameCid, name: String },
  Int { prev: NameCid, int: BigUint },
}

impl IpldEmbed for Name {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Anon => Ipld::List(vec![Ipld::String(String::from("#NA"))]),
      Self::Str { prev, name } => Ipld::List(vec![
        Ipld::String(String::from("#NS")),
        prev.to_ipld(),
        Ipld::String(name.clone()),
      ]),
      Self::Int { prev, int } => Ipld::List(vec![
        Ipld::String(String::from("#NI")),
        prev.to_ipld(),
        Ipld::Bytes(int.to_bytes_be()),
      ]),
    }
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      NAME_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [String(tag)] if tag == "#NA" => Ok(Name::Anon),
        [String(tag), p, String(n)] if tag == "#NS" => {
          let prev = NameCid::from_ipld(p)?;
          Ok(Name::Str { prev, name: n.clone() })
        }
        [String(tag), p, Bytes(i)] if tag == "#NI" => {
          let prev = NameCid::from_ipld(p)?;
          Ok(Name::Int { prev, int: BigUint::from_bytes_be(&i) })
        }
        xs => Err(IpldError::ExpectedName(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedName(xs.to_owned())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::{
    format::tests::arbitrary_cid,
    tests::{
      arbitrary_big_uint,
      frequency,
    },
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for NameCid {
    fn arbitrary(g: &mut Gen) -> Self {
      let cid = arbitrary_cid(g, NAME_CODEC);
      NameCid { hash: *cid.hash() }
    }
  }

  impl Arbitrary for Name {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Name>)> = vec![
        (1, Box::new(|_| Name::Anon)),
        (
          1,
          Box::new(|g| Name::Str {
            prev: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Name::Int {
            prev: Arbitrary::arbitrary(g),
            int: arbitrary_big_uint()(g),
          }),
        ),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn name_ipld(x: Name) -> bool {
    match Name::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

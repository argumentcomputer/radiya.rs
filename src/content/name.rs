use crate::content::ipld::{
  Cid,
  IpldEmbed,
  IpldError,
  NameCid,
  NAME,
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

impl IpldEmbed for Name {
  const CODEC: u64 = NAME;

  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Anon => Ipld::List(vec![Ipld::String(String::from("#NA"))]),
      Self::Str { prev, name } => Ipld::List(vec![
        Ipld::String(String::from("#NS")),
        Ipld::Link(prev.to_dyn()),
        Ipld::String(name.clone()),
      ]),
      Self::Int { prev, int } => Ipld::List(vec![
        Ipld::String(String::from("#NI")),
        Ipld::Link(prev.to_dyn()),
        Ipld::Bytes(int.to_bytes_be()),
      ]),
    }
  }

  fn cid(&self) -> Cid<{ Self::CODEC }> {
    Cid {
      hash: Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [String(tag)] if tag == "#NA" => Ok(Name::Anon),
        [String(tag), Link(p), String(n)] if tag == "#NS" => {
          let prev = Cid::from_dyn(p)?;
          Ok(Name::Str { prev, name: n.clone() })
        }
        [String(tag), Link(p), Bytes(i)] if tag == "#NI" => {
          let prev = NameCid::from_dyn(p)?;
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
    content::tests::frequency,
    tests::arbitrary_big_uint,
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

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

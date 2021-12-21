use crate::content::{
  cid::{
    NameCid,
    NAME,
  },
  ipld::{
    IpldEmbed,
    IpldError,
  },
};
use alloc::{
  borrow::ToOwned,
  string::String,
};
use num_bigint::BigUint;
use sp_ipld::Ipld;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Name {
  Anon,
  Str { prev: NameCid, name: String },
  Int { prev: NameCid, int: BigUint },
}

impl IpldEmbed for Name {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Anon => {
        Ipld::List(vec![Ipld::Integer(NAME.into()), Ipld::Integer(0)])
      }
      Self::Str { prev, name } => Ipld::List(vec![
        Ipld::Integer(NAME.into()),
        Ipld::Integer(1),
        prev.to_ipld(),
        name.to_ipld(),
      ]),
      Self::Int { prev, int } => Ipld::List(vec![
        Ipld::Integer(NAME.into()),
        Ipld::Integer(2),
        prev.to_ipld(),
        int.to_ipld(),
      ]),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use alloc::string;
    use Ipld::*;
    let tag: i128 = NAME.into();
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0)] if *t == tag => Ok(Name::Anon),
        [Integer(t), Integer(1), p, n] if *t == tag => {
          let prev = NameCid::from_ipld(p)?;
          let name = string::String::from_ipld(n)?;
          Ok(Name::Str { prev, name })
        }
        [Integer(t), Integer(2), p, i] if *t == tag => {
          let prev = NameCid::from_ipld(p)?;
          let int = BigUint::from_ipld(i)?;
          Ok(Name::Int { prev, int })
        }

        xs => Err(IpldError::expected("Name", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("Name", xs)),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::tests::{
    arbitrary_big_uint,
    frequency,
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

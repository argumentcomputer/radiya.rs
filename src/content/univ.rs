use sp_std::{
  fmt,
  fmt::Display,
};

use crate::content::{
  cid::{
    NameCid,
    UnivCid,
    UnivMetaCid,
    UNIV,
    UNIV_META,
  },
  ipld::{
    IpldEmbed,
    IpldError,
  },
};
use alloc::borrow::ToOwned;

use num_bigint::BigUint;
use sp_ipld::Ipld;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Univ {
  Zero,
  Succ { pred: UnivCid },
  Max { lhs: UnivCid, rhs: UnivCid },
  IMax { lhs: UnivCid, rhs: UnivCid },
  Param { idx: BigUint },
}

impl Display for Univ {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Zero => {
        write!(f, "#Zero")
      }
      Self::Succ { pred } => {
        write!(f, "#Succ {}", pred)
      }
      Self::Max { lhs, rhs } => {
        write!(f, "#Max {} {}", lhs, rhs)
      }
      Self::IMax { lhs, rhs } => {
        write!(f, "#IMax {} {}", lhs, rhs)
      }
      Self::Param { idx } => {
        write!(f, "#Param {}", idx)
      }
    }
  }
}

impl IpldEmbed for Univ {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Zero => {
        Ipld::List(vec![Ipld::Integer(UNIV.into()), Ipld::Integer(0)])
      }
      Self::Succ { pred } => Ipld::List(vec![
        Ipld::Integer(UNIV.into()),
        Ipld::Integer(1),
        pred.to_ipld(),
      ]),
      Self::Max { lhs, rhs } => Ipld::List(vec![
        Ipld::Integer(UNIV.into()),
        Ipld::Integer(2),
        lhs.to_ipld(),
        rhs.to_ipld(),
      ]),
      Self::IMax { lhs, rhs } => Ipld::List(vec![
        Ipld::Integer(UNIV.into()),
        Ipld::Integer(3),
        lhs.to_ipld(),
        rhs.to_ipld(),
      ]),
      Self::Param { idx } => Ipld::List(vec![
        Ipld::Integer(UNIV.into()),
        Ipld::Integer(4),
        idx.to_ipld(),
      ]),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = UNIV.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0)] if *t == tag => Ok(Univ::Zero),
        [Integer(t), Integer(1), p] if *t == tag => {
          let pred = UnivCid::from_ipld(p)?;
          Ok(Univ::Succ { pred })
        }
        [Integer(t), Integer(2), l, r] if *t == tag => {
          let lhs = UnivCid::from_ipld(l)?;
          let rhs = UnivCid::from_ipld(r)?;
          Ok(Univ::Max { lhs, rhs })
        }
        [Integer(t), Integer(3), l, r] if *t == tag => {
          let lhs = UnivCid::from_ipld(l)?;
          let rhs = UnivCid::from_ipld(r)?;
          Ok(Univ::IMax { lhs, rhs })
        }
        [Integer(t), Integer(4), idx] if *t == tag => {
          let idx = BigUint::from_ipld(idx)?;
          Ok(Univ::Param { idx })
        }
        xs => Err(IpldError::expected("Univ", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("Univ", xs)),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnivMeta {
  Zero,
  Succ(UnivMetaCid),
  Max(UnivMetaCid, UnivMetaCid),
  IMax(UnivMetaCid, UnivMetaCid),
  Param(NameCid),
}

impl Display for UnivMeta {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Zero => {
        write!(f, "#Zero")
      }
      Self::Succ(pred) => {
        write!(f, "#Succ {}", pred)
      }
      Self::Max(lhs, rhs) => {
        write!(f, "#Max {} {}", lhs, rhs)
      }
      Self::IMax(lhs, rhs) => {
        write!(f, "#IMax {} {}", lhs, rhs)
      }
      Self::Param(nam) => {
        write!(f, "#Param {}", nam)
      }
    }
  }
}

impl IpldEmbed for UnivMeta {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Zero => {
        Ipld::List(vec![Ipld::Integer(UNIV_META.into()), Ipld::Integer(0)])
      }
      Self::Succ(pred) => Ipld::List(vec![
        Ipld::Integer(UNIV_META.into()),
        Ipld::Integer(1),
        pred.to_ipld(),
      ]),
      Self::Max(lhs, rhs) => Ipld::List(vec![
        Ipld::Integer(UNIV_META.into()),
        Ipld::Integer(2),
        lhs.to_ipld(),
        rhs.to_ipld(),
      ]),
      Self::IMax(lhs, rhs) => Ipld::List(vec![
        Ipld::Integer(UNIV_META.into()),
        Ipld::Integer(3),
        lhs.to_ipld(),
        rhs.to_ipld(),
      ]),
      Self::Param(nam) => Ipld::List(vec![
        Ipld::Integer(UNIV_META.into()),
        Ipld::Integer(4),
        nam.to_ipld(),
      ]),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = UNIV_META.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0)] if *t == tag => Ok(UnivMeta::Zero),
        [Integer(t), Integer(1), pred] if *t == tag => {
          let pred = UnivMetaCid::from_ipld(pred)?;
          Ok(UnivMeta::Succ(pred))
        }
        [Integer(t), Integer(2), lhs, rhs] if *t == tag => {
          let lhs = UnivMetaCid::from_ipld(lhs)?;
          let rhs = UnivMetaCid::from_ipld(rhs)?;
          Ok(UnivMeta::Max(lhs, rhs))
        }
        [Integer(t), Integer(3), lhs, rhs] if *t == tag => {
          let lhs = UnivMetaCid::from_ipld(lhs)?;
          let rhs = UnivMetaCid::from_ipld(rhs)?;
          Ok(UnivMeta::IMax(lhs, rhs))
        }
        [Integer(t), Integer(4), nam] if *t == tag => {
          let nam = NameCid::from_ipld(nam)?;
          Ok(UnivMeta::Param(nam))
        }
        xs => Err(IpldError::expected("UnivMeta", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("UnivMeta", xs)),
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
  impl Arbitrary for UnivMeta {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> UnivMeta>)> = vec![
        (1, Box::new(|_| UnivMeta::Zero)),
        (1, Box::new(|g| UnivMeta::Succ(Arbitrary::arbitrary(g)))),
        (
          1,
          Box::new(|g| {
            UnivMeta::Max(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g))
          }),
        ),
        (
          1,
          Box::new(|g| {
            UnivMeta::IMax(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g))
          }),
        ),
        (1, Box::new(|g| UnivMeta::Param(Arbitrary::arbitrary(g)))),
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

  #[quickcheck]
  fn univ_meta_ipld(x: UnivMeta) -> bool {
    match UnivMeta::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

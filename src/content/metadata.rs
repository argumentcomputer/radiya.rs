use crate::{
  content::{
    cid::{
      MetaCid,
      NameCid,
      METADATA,
    },
    ipld::{
      IpldEmbed,
      IpldError,
    },
  },
  parse::position::Position,
};
use alloc::{
  borrow::ToOwned,
  string::String,
};

use sp_ipld::Ipld;
use sp_std::vec::Vec;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Metadata {
  Leaf,
  Node {
    pos: Option<Position>,
    name: Option<NameCid>,
    link: Option<MetaCid>,
    misc: String,
    branch: Vec<MetaCid>,
  },
}

impl IpldEmbed for Metadata {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Leaf => {
        Ipld::List(vec![Ipld::Integer(METADATA.into()), Ipld::Integer(0)])
      }
      Self::Node { pos, name, link, misc, branch } => Ipld::List(vec![
        Ipld::Integer(METADATA.into()),
        Ipld::Integer(1),
        pos.to_ipld(),
        name.to_ipld(),
        link.to_ipld(),
        misc.to_ipld(),
        branch.to_ipld(),
      ]),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = METADATA.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0)] if *t == tag => Ok(Metadata::Leaf),
        [Integer(t), Integer(1), pos, name, link, misc, branch]
          if *t == tag =>
        {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let link = IpldEmbed::from_ipld(link)?;
          let misc = IpldEmbed::from_ipld(misc)?;
          let branch: Vec<MetaCid> = IpldEmbed::from_ipld(branch)?;
          Ok(Metadata::Node { pos, name, link, misc, branch })
        }
        xs => Err(IpldError::expected("Metadata", &Ipld::List(xs.to_owned()))),
      },
      x => Err(IpldError::expected("Metadata", x)),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::{
    content::tests::frequency,
    tests::gen_range,
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for Metadata {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Metadata>)> = vec![
        (1, Box::new(|_| Metadata::Leaf)),
        (
          1,
          Box::new(|g| {
            let mut branch = Vec::new();
            let num = gen_range(g, 0..6);
            for _ in 0..num {
              branch.push(Arbitrary::arbitrary(g));
            }
            Metadata::Node {
              pos: Arbitrary::arbitrary(g),
              name: Arbitrary::arbitrary(g),
              link: Arbitrary::arbitrary(g),
              misc: Arbitrary::arbitrary(g),
              branch,
            }
          }),
        ),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn metadata_ipld(x: Metadata) -> bool {
    match Metadata::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

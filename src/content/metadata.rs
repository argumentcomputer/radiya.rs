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
  parse::position::Pos,
};
use alloc::{
  borrow::ToOwned,
  string::String,
};

use sp_ipld::Ipld;
use sp_std::vec::Vec;

pub enum ExprMeta {
  Var(Pos),
  Sort(Pos, ExprMetaCid),
  Const(Pos, NameCid, ConstMetaCid, Vec<UnivMetaCid>),
  App(Pos, ExprMetaCid, ExprMetaCid),
  Lam(Pos, NameCid, ExprMetaCid, ExprMetaCid),
}

pub enum ConstMeta {}

impl Metadata {
  pub fn leaf(pos: Option<Position>) -> Self {
    Metadata {
      pos,
      name: None,
      link: None,
      params: vec![],
      misc: vec![],
      branch: vec![],
    }
  }

  pub fn refer(
    pos: Option<Position>,
    name: NameCid,
    link: MetaCid,
    xs: Vec<MetaCid>,
  ) -> Self {
    Metadata {
      pos,
      name: Some(name),
      link: Some(link),
      params: vec![],
      misc: String::from(""),
      branch: xs,
    }
  }

  pub fn constant(
    pos: Option<Position>,
    name: NameCid,
    params: Vec<NameCid>,
    xs: Vec<MetaCid>,
  ) -> Self {
    Metadata {
      pos,
      name: Some(name),
      link: None,
      params,
      misc: String::from(""),
      branch: xs,
    }
  }

  pub fn name(pos: Option<Position>, name: NameCid, xs: Vec<MetaCid>) -> Self {
    Metadata {
      pos,
      name: Some(name),
      link: None,
      params: vec![],
      misc: String::from(""),
      branch: xs,
    }
  }

  pub fn branch(pos: Option<Position>, xs: Vec<MetaCid>) -> Self {
    Metadata {
      pos,
      name: None,
      link: None,
      params: vec![],
      misc: String::from(""),
      branch: xs,
    }
  }
}

impl IpldEmbed for Metadata {
  fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      Ipld::Integer(METADATA.into()),
      self.pos.to_ipld(),
      self.name.to_ipld(),
      self.link.to_ipld(),
      self.params.to_ipld(),
      self.misc.to_ipld(),
      self.branch.to_ipld(),
    ])
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = METADATA.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), pos, name, link, params, misc, branch] if *t == tag => {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let link = IpldEmbed::from_ipld(link)?;
          let params: Vec<NameCid> = IpldEmbed::from_ipld(params)?;
          let misc = IpldEmbed::from_ipld(misc)?;
          let branch: Vec<MetaCid> = IpldEmbed::from_ipld(branch)?;
          Ok(Metadata { pos, name, link, params, misc, branch })
        }
        xs => Err(IpldError::expected("Metadata", &Ipld::List(xs.to_owned()))),
      },
      x => Err(IpldError::expected("Metadata", x)),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::tests::gen_range;

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for Metadata {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut branch = Vec::new();
      let num = gen_range(g, 0..6);
      for _ in 0..num {
        branch.push(Arbitrary::arbitrary(g));
      }
      Metadata {
        pos: Arbitrary::arbitrary(g),
        name: Arbitrary::arbitrary(g),
        link: Arbitrary::arbitrary(g),
        misc: Arbitrary::arbitrary(g),
        branch,
      }
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

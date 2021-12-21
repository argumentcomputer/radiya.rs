use alloc::string::String;
use sp_cid::Cid;
use sp_ipld::Ipld;
use sp_multihash::Multihash;

use crate::content::{
  constant::Const,
  expr::Expr,
  name::Name,
  univ::{
    Univ,
    UnivMeta,
  },
};

use crate::expression::Literal;

use super::ipld::{
  cid,
  IpldEmbed,
  IpldError,
};
pub const LITERAL: u64 = 0xC0DE_0000;
pub const NAME: u64 = 0x3E7A_0000;
pub const UNIV: u64 = 0xC0DE_0001;
pub const UNIV_META: u64 = 0x3E7A_0001;
pub const EXPR: u64 = 0xC0DE_0002;
pub const EXPR_META: u64 = 0x3E7A_0002;
pub const CONST: u64 = 0xC0DE_0003;
pub const CONST_META: u64 = 0x3E7A_0003;
pub const ENV: u64 = 0xC0DE_0004;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LitCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivMetaCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprMetaCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstMetaCid(pub Cid);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnvCid(pub Cid);

impl LitCid {
  pub fn new(constant: &Literal) -> Self {
    LitCid(cid(LITERAL, &constant.to_ipld()))
  }
}

impl IpldEmbed for LitCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == LITERAL => Ok(LitCid(*cid)),
      x => Err(IpldError::Expected(String::from("LiteralCid"), x.clone())),
    }
  }
}

impl NameCid {
  pub fn new(name: &Name) -> Self { NameCid(cid(NAME, &name.to_ipld())) }
}

impl IpldEmbed for NameCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == NAME => Ok(NameCid(*cid)),
      x => Err(IpldError::Expected(String::from("NameCid"), x.clone())),
    }
  }
}

impl UnivCid {
  pub fn new(univ: &Univ) -> Self { UnivCid(cid(UNIV, &univ.to_ipld())) }
}

impl IpldEmbed for UnivCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == UNIV => Ok(UnivCid(*cid)),
      x => Err(IpldError::Expected(String::from("UnivCid"), x.clone())),
    }
  }
}
impl UnivMetaCid {
  pub fn new(univ: &UnivMeta) -> Self {
    UnivMetaCid(cid(UNIV, &univ.to_ipld()))
  }
}

impl IpldEmbed for UnivMetaCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == UNIV_META => Ok(UnivMetaCid(*cid)),
      x => Err(IpldError::Expected(String::from("UnivMetaCid"), x.clone())),
    }
  }
}

impl ExprCid {
  pub fn new(expr: &Expr) -> Self { ExprCid(cid(EXPR, &expr.to_ipld())) }
}

impl IpldEmbed for ExprCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == EXPR => Ok(ExprCid(*cid)),
      x => Err(IpldError::Expected(String::from("ExprCid"), x.clone())),
    }
  }
}

impl ConstCid {
  pub fn new(constant: &Const) -> Self {
    ConstCid(cid(CONST, &constant.to_ipld()))
  }
}

impl IpldEmbed for ConstCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == CONST => Ok(ConstCid(*cid)),
      x => Err(IpldError::Expected(String::from("ConstCid"), x.clone())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use sp_cid::Cid;
  use sp_multihash::{
    Code,
    MultihashDigest,
  };

  use super::*;

  pub fn arbitrary_cid(g: &mut Gen, code: u64) -> Cid {
    let mut bytes: [u8; 32] = [0; 32];
    for x in bytes.iter_mut() {
      *x = Arbitrary::arbitrary(g);
    }
    Cid::new_v1(code, Code::Blake3_256.digest(&bytes))
  }
  impl Arbitrary for NameCid {
    fn arbitrary(g: &mut Gen) -> Self { NameCid(arbitrary_cid(g, NAME)) }
  }
  impl Arbitrary for UnivCid {
    fn arbitrary(g: &mut Gen) -> Self { UnivCid(arbitrary_cid(g, UNIV)) }
  }
  impl Arbitrary for UnivMetaCid {
    fn arbitrary(g: &mut Gen) -> Self {
      UnivMetaCid(arbitrary_cid(g, UNIV_META))
    }
  }
  impl Arbitrary for ExprCid {
    fn arbitrary(g: &mut Gen) -> Self { ExprCid(arbitrary_cid(g, EXPR)) }
  }
  impl Arbitrary for ExprMetaCid {
    fn arbitrary(g: &mut Gen) -> Self {
      ExprMetaCid(arbitrary_cid(g, EXPR_META))
    }
  }
  impl Arbitrary for ConstCid {
    fn arbitrary(g: &mut Gen) -> Self { ConstCid(arbitrary_cid(g, CONST)) }
  }
  impl Arbitrary for ConstMetaCid {
    fn arbitrary(g: &mut Gen) -> Self {
      ConstMetaCid(arbitrary_cid(g, CONST_META))
    }
  }
}

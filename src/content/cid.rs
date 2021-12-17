use alloc::string::String;
use sp_cid::Cid;
use sp_ipld::Ipld;
use sp_multihash::Multihash;

use super::ipld::{
  IpldEmbed,
  IpldError,
};

// Prefix 0x10DE is hexspeak for LODE, since these codecs are the
// content-addressed veins of meaning in the Truth Mines.
pub const METADATA: u64 = 0x10DE_0001;
pub const NAME: u64 = 0x10DE_0002;
pub const UNIVERSE: u64 = 0x10DE_0003;
pub const EXPRESSION: u64 = 0x10DE_0004;
pub const LITERAL: u64 = 0x10DE_0005;
pub const CONSTANT: u64 = 0x10DE_0006;
pub const ENVIRONMENT: u64 = 0x10DE_0007;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MetaCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiteralCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnvironmentCid(pub Cid);

impl MetaCid {
  pub fn new(hash: Multihash) -> Self { MetaCid(Cid::new_v1(METADATA, hash)) }
}

impl IpldEmbed for MetaCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == METADATA => Ok(MetaCid(*cid)),
      x => Err(IpldError::Expected(String::from("MetaCid"), x.clone())),
    }
  }
}

impl NameCid {
  pub fn new(hash: Multihash) -> Self { NameCid(Cid::new_v1(NAME, hash)) }
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
  pub fn new(hash: Multihash) -> Self { UnivCid(Cid::new_v1(UNIVERSE, hash)) }
}

impl IpldEmbed for UnivCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == UNIVERSE => Ok(UnivCid(*cid)),
      x => Err(IpldError::Expected(String::from("UnivCid"), x.clone())),
    }
  }
}

impl ExprCid {
  pub fn new(hash: Multihash) -> Self { ExprCid(Cid::new_v1(EXPRESSION, hash)) }
}

impl IpldEmbed for ExprCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == EXPRESSION => Ok(ExprCid(*cid)),
      x => Err(IpldError::Expected(String::from("ExprCid"), x.clone())),
    }
  }
}

impl ConstCid {
  pub fn new(hash: Multihash) -> Self { ConstCid(Cid::new_v1(CONSTANT, hash)) }
}

impl IpldEmbed for ConstCid {
  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid) if cid.codec() == CONSTANT => Ok(ConstCid(*cid)),
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
    fn arbitrary(g: &mut Gen) -> Self { UnivCid(arbitrary_cid(g, UNIVERSE)) }
  }
  impl Arbitrary for ExprCid {
    fn arbitrary(g: &mut Gen) -> Self { ExprCid(arbitrary_cid(g, EXPRESSION)) }
  }

  impl Arbitrary for MetaCid {
    fn arbitrary(g: &mut Gen) -> Self { MetaCid(arbitrary_cid(g, METADATA)) }
  }
  impl Arbitrary for ConstCid {
    fn arbitrary(g: &mut Gen) -> Self { ConstCid(arbitrary_cid(g, CONSTANT)) }
  }
}

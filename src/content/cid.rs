use sp_cid::Cid;
use sp_multihash::Multihash;

use super::ipld_error::IpldError;

const META: u64 = 0x1ea7_0001;
const NAME: u64 = 0x1ea7_0002;
const UNIV: u64 = 0x1ea7_0003;
const EXPR: u64 = 0x1ea7_0004;
const CONST: u64 = 0x1ea7_0005;

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

impl MetaCid {
  pub fn new(hash: Multihash) -> Self { MetaCid(Cid::new_v1(META, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == META {
      Ok(MetaCid(cid))
    }
    else {
      Err(IpldError::MetaCid(cid))
    }
  }
}

impl NameCid {
  pub fn new(hash: Multihash) -> Self { NameCid(Cid::new_v1(NAME, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == NAME {
      Ok(NameCid(cid))
    }
    else {
      Err(IpldError::NameCid(cid))
    }
  }
}

impl UnivCid {
  pub fn new(hash: Multihash) -> Self { UnivCid(Cid::new_v1(UNIV, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == UNIV {
      Ok(UnivCid(cid))
    }
    else {
      Err(IpldError::UnivCid(cid))
    }
  }
}

impl ExprCid {
  pub fn new(hash: Multihash) -> Self { ExprCid(Cid::new_v1(EXPR, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == EXPR {
      Ok(ExprCid(cid))
    }
    else {
      Err(IpldError::ExprCid(cid))
    }
  }
}

impl ConstCid {
  pub fn new(hash: Multihash) -> Self { ConstCid(Cid::new_v1(CONST, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == CONST {
      Ok(ConstCid(cid))
    }
    else {
      Err(IpldError::ConstCid(cid))
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
  impl Arbitrary for ExprCid {
    fn arbitrary(g: &mut Gen) -> Self { ExprCid(arbitrary_cid(g, EXPR)) }
  }
  impl Arbitrary for ConstCid {
    fn arbitrary(g: &mut Gen) -> Self { ConstCid(arbitrary_cid(g, CONST)) }
  }
}

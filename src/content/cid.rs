use sp_cid::Cid;
use sp_multihash::Multihash;

use super::ipld_error::IpldError;

const NAME_SPACE: u64 = 0x6c71c4;
const UNIV_SPACE: u64 = 0x268bd2;
const EXPR_SPACE: u64 = 0x2aa198;
const DECL_SPACE: u64 = 0x859900;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeclCid(pub Cid);

impl NameCid {
  pub fn new(hash: Multihash) -> Self { NameCid(Cid::new_v1(NAME_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == NAME_SPACE {
      Ok(NameCid(cid))
    }
    else {
      Err(IpldError::NameCid(cid))
    }
  }
}

impl UnivCid {
  pub fn new(hash: Multihash) -> Self { UnivCid(Cid::new_v1(UNIV_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == UNIV_SPACE {
      Ok(UnivCid(cid))
    }
    else {
      Err(IpldError::UnivCid(cid))
    }
  }
}

impl ExprCid {
  pub fn new(hash: Multihash) -> Self { ExprCid(Cid::new_v1(EXPR_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == EXPR_SPACE {
      Ok(ExprCid(cid))
    }
    else {
      Err(IpldError::ExprCid(cid))
    }
  }
}

impl DeclCid {
  pub fn new(hash: Multihash) -> Self { DeclCid(Cid::new_v1(DECL_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Result<Self, IpldError> {
    if cid.codec() == DECL_SPACE {
      Ok(DeclCid(cid))
    }
    else {
      Err(IpldError::DeclCid(cid))
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
    fn arbitrary(g: &mut Gen) -> Self { NameCid(arbitrary_cid(g, NAME_SPACE)) }
  }
  impl Arbitrary for UnivCid {
    fn arbitrary(g: &mut Gen) -> Self { UnivCid(arbitrary_cid(g, UNIV_SPACE)) }
  }
  impl Arbitrary for ExprCid {
    fn arbitrary(g: &mut Gen) -> Self { ExprCid(arbitrary_cid(g, EXPR_SPACE)) }
  }
  impl Arbitrary for DeclCid {
    fn arbitrary(g: &mut Gen) -> Self { DeclCid(arbitrary_cid(g, DECL_SPACE)) }
  }
}

use sp_cid::{
  Cid,
  Version,
};

use sp_ipld::Ipld;
use sp_std::{
  num::TryFromIntError,
  vec::Vec,
};

pub trait IpldEmbed: Sized {
  fn to_ipld(&self) -> Ipld;
  fn cid(&self) -> Cid;
  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError>;
}

#[derive(PartialEq, Debug, Clone)]
pub enum IpldError {
  Utf8(Vec<u8>, alloc::string::FromUtf8Error),
  ByteCount(Vec<u8>, u64),
  UnicodeChar(u32),
  U64(TryFromIntError),
  U32(TryFromIntError),
  UnexpectedCidCodec(Cid, Version, u64),
  ExpectedName(Ipld),
  ExpectedNameCid(Ipld),
  ExpectedUniv(Ipld),
  ExpectedUnivCid(Ipld),
  ExpectedBinderInfo(Ipld),
  ExpectedExpr(Ipld),
  ExpectedDef(Ipld),
  ExpectedReducibilityHints(Ipld),
  ExpectedDefinitionSafety(Ipld),
  ExpectedAxiom(Ipld),
  ExpectedTheorem(Ipld),
  ExpectedOpaque(Ipld),
  ExpectedConstructor(Ipld),
  ExpectedInductiveType(Ipld),
  ExpectedDecl(Ipld),
}

//#[cfg(test)]
// pub mod tests {
//  use quickcheck::{
//    Arbitrary,
//    Gen,
//  };
//  use sp_multihash::{
//    Code,
//    MultihashDigest,
//  };
//
//  use super::*;
//
//  pub fn arbitrary_cid<const C: u64>(g: &mut Gen, code: u64) -> Cid<C> {
//    let mut bytes: [u8; 32] = [0; 32];
//    for x in bytes.iter_mut() {
//      *x = Arbitrary::arbitrary(g);
//    }
//    Cid::new(Code::Blake3_256.digest(&bytes))
//  }
//
//  impl<const C: u64> Arbitrary for Cid<C> {
//    fn arbitrary(g: &mut Gen) -> Self { arbitrary_cid(g, C) }
//  }
//}

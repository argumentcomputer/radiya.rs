use sp_cid::{
  Cid,
  Codec,
  Version,
};

use sp_ipld::Ipld;
use sp_multihash::Multihash;
use sp_std::{
  num::TryFromIntError,
  vec::Vec,
};

pub const META: u32 = 0x7EA4_4E7A;
pub const NOTN: u32 = 0x7EA4_4074;
pub const NAME: u32 = 0x7EA4_4A4E;
pub const UNIV: u32 = 0x7EA4_1E71;
pub const EXPR: u32 = 0x7EA4_E712;
pub const DECL: u32 = 0x7EA4_DEC1;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NotnCid {
  pub hash: Multihash,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MetaCid {
  pub hash: Multihash,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameCid {
  pub hash: Multihash,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivCid {
  pub hash: Multihash,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprCid {
  pub hash: Multihash,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeclCid {
  pub indx: u32,
  pub hash: Multihash,
}

// impl<const C: Codec> Cid<C> {
//  pub fn new(hash: Multihash) -> Self { Self { hash } }
//
//  pub fn to_dyn(&self) -> DynCid { DynCid::new_v1(C, self.hash) }
//
//  pub fn from_dyn(cid: &DynCid) -> Result<Self, IpldError> {
//    if cid.version() == Version::V1 && C == cid.codec() {
//      Ok(Self { hash: *cid.hash() })
//    }
//    else {
//      Err(IpldError::UnexpectedCidCodec(*cid, cid.version(), cid.codec()))
//    }
//  }
//}
// pub type NameCid = Cid<NAME>;
// pub type NotnCid = Cid<NOTN>;
// pub type UnivCid = Cid<UNIV>;
// pub type ExprCid = Cid<EXPR>;
// pub type CnstCid = Cid<DECL>;
//
// pub trait IpldEmbed: Sized {
//  const CODEC: u64;
//  fn to_ipld(&self) -> Ipld;
//  fn cid(&self) -> Cid<{ Self::CODEC }>;
//  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError>;
//}
//
//#[derive(PartialEq, Debug, Clone)]
// pub enum IpldError {
//  Utf8(Vec<u8>, alloc::string::FromUtf8Error),
//  ByteCount(Vec<u8>, u64),
//  UnicodeChar(u32),
//  U64(TryFromIntError),
//  UnexpectedCidCodec(DynCid, Version, u64),
//  ExpectedName(Ipld),
//  ExpectedUniv(Ipld),
//  ExpectedBinderInfo(Ipld),
//  ExpectedExpr(Ipld),
//}
//
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
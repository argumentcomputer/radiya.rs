// use sp_cid::{
//  Cid,
//  Codec,
//};
// use sp_ipld::Ipld;
// use sp_multihash::Multihash;
//
// use alloc::string::String;
//
// use crate::content::ipld::{
//  IpldEmbed,
//  IpldError,
//};
// impl<const S: usize, const C: Codec> TypedCid<S, C> {
//  pub fn to_dynamic(&self) -> Cid<S> { Cid::new_v1(C, self.hash) }
//
//  pub fn from_dynamic(cid: &Cid<S>) -> Option<Self> {
//    if cid.version() == Version::V1 && C == cid.codec() {
//      Some(Self { hash: *cid.hash() })
//    }
//    else {
//      None
//    }
//  }
//}
// pub struct NameCid(pub Cid);
//#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// pub struct UnivCid(pub Cid);
//#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// pub struct ExprCid(pub Cid);
//#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// pub struct DeclCid(pub Cid);
//
// impl NameCid {
//  pub fn new(hash: Multihash) -> Self { NameCid(Cid::new_v1(NAME_SPACE, hash))
// }
//}
// impl IpldEmbed for NameCid {
//  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }
//
//  fn cid(&self) -> Cid { self.0 }
//
//  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
//    match ipld {
//      Ipld::Link(cid) if cid.codec() == NAME_SPACE => Ok(NameCid(*cid)),
//      _ => {
//        Err(IpldError::UnexpectedIpld(ipld.clone(), String::from("NameCid")))
//      }
//    }
//  }
//}
// impl UnivCid {
//  pub fn new(hash: Multihash) -> Self { UnivCid(Cid::new_v1(UNIV_SPACE, hash))
// }
//}
// impl IpldEmbed for UnivCid {
//  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }
//
//  fn cid(&self) -> Cid { self.0 }
//
//  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
//    match ipld {
//      Ipld::Link(cid) if cid.codec() == UNIV_SPACE => Ok(UnivCid(*cid)),
//      _ => {
//        Err(IpldError::UnexpectedIpld(ipld.clone(), String::from("UnivCid")))
//      }
//    }
//  }
//}
// impl ExprCid {
//  pub fn new(hash: Multihash) -> Self { ExprCid(Cid::new_v1(EXPR_SPACE, hash))
// }
//}
// impl IpldEmbed for ExprCid {
//  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }
//
//  fn cid(&self) -> Cid { self.0 }
//
//  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
//    match ipld {
//      Ipld::Link(cid) if cid.codec() == EXPR_SPACE => Ok(ExprCid(*cid)),
//      _ => {
//        Err(IpldError::UnexpectedIpld(ipld.clone(), String::from("ExprCid")))
//      }
//    }
//  }
//}
// impl DeclCid {
//  pub fn new(hash: Multihash) -> Self { DeclCid(Cid::new_v1(DECL_SPACE, hash))
// }
//}
// impl IpldEmbed for DeclCid {
//  fn to_ipld(&self) -> Ipld { Ipld::Link(self.0) }
//
//  fn cid(&self) -> Cid { self.0 }
//
//  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
//    match ipld {
//      Ipld::Link(cid) if cid.codec() == DECL_SPACE => Ok(DeclCid(*cid)),
//      _ => {
//        Err(IpldError::UnexpectedIpld(ipld.clone(), String::from("DeclCid")))
//      }
//    }
//  }
//}
//
//#[cfg(test)]
// pub mod tests {
//  use quickcheck::{
//    Arbitrary,
//    Gen,
//  };
//  use sp_cid::Cid;
//  use sp_multihash::{
//    Code,
//    MultihashDigest,
//  };
//
//  use super::*;
//
//  pub fn arbitrary_cid(g: &mut Gen, code: u64) -> Cid {
//    let mut bytes: [u8; 32] = [0; 32];
//    for x in bytes.iter_mut() {
//      *x = Arbitrary::arbitrary(g);
//    }
//    Cid::new_v1(code, Code::Blake3_256.digest(&bytes))
//  }
//  impl Arbitrary for NameCid {
//    fn arbitrary(g: &mut Gen) -> Self { NameCid(arbitrary_cid(g, NAME_SPACE))
// }  }
//  impl Arbitrary for UnivCid {
//    fn arbitrary(g: &mut Gen) -> Self { UnivCid(arbitrary_cid(g, UNIV_SPACE))
// }  }
//  impl Arbitrary for ExprCid {
//    fn arbitrary(g: &mut Gen) -> Self { ExprCid(arbitrary_cid(g, EXPR_SPACE))
// }  }
//  impl Arbitrary for DeclCid {
//    fn arbitrary(g: &mut Gen) -> Self { DeclCid(arbitrary_cid(g, DECL_SPACE))
// }  }
//}

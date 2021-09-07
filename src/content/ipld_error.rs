use sp_cid::Cid;
use sp_ipld::Ipld;
use sp_std::{
  num::TryFromIntError,
  vec::Vec,
};

#[derive(PartialEq, Debug, Clone)]
pub enum IpldError {
  Utf8(Vec<u8>, alloc::string::FromUtf8Error),
  ByteCount(Vec<u8>, u64),
  UnicodeChar(u32),
  U64(TryFromIntError),
  Name(Ipld),
  NameCid(Cid),
  Univ(Ipld),
  UnivCid(Cid),
  Expr(Ipld),
  Bind(Ipld),
  ExprCid(Cid),
  Decl(Ipld),
  Notation(Ipld),
  DeclCid(Cid),
  Intro(Ipld),
}

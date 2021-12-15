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
  Meta(Ipld),
  MetaCid(Cid),
  Name(Ipld),
  NameCid(Cid),
  Univ(Ipld),
  UnivCid(Cid),
  Expr(Ipld),
  BinderInfo(Ipld),
  Literal(Ipld),
  ExprCid(Cid),
  Const(Ipld),
  QuotKind(Ipld),
  DefinitionSafety(Ipld),
  RecursorRule(Ipld),
  Notation(Ipld),
  ConstCid(Cid),
  Intro(Ipld),
}

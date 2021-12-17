use alloc::string::String;
use num_bigint::BigUint;
use sp_cid::Cid;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
  Ipld,
};
use sp_multihash::{
  Code,
  MultihashDigest,
};
use sp_std::{
  convert::TryInto,
  num::TryFromIntError,
  vec::Vec,
};

pub const DAGCBOR: u64 = 0x71;

pub trait IpldEmbed: Sized {
  fn to_ipld(&self) -> Ipld;
  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError>;
}

pub fn cid_from_ipld(code: u64, ipld: &Ipld) -> Cid {
  Cid::new_v1(
    code,
    Code::Blake3_256
      .digest(DagCborCodec.encode(ipld).unwrap().into_inner().as_ref()),
  )
}

#[derive(PartialEq, Debug, Clone)]
pub enum IpldError {
  Utf8(Vec<u8>, alloc::string::FromUtf8Error),
  ByteCount(Vec<u8>, u64),
  UnicodeChar(u32),
  U64(TryFromIntError),
  Expected(String, Ipld),
}

impl IpldError {
  pub fn expected(s: &str, ipld: &Ipld) -> IpldError {
    IpldError::Expected(String::from(s), ipld.clone())
  }
}

impl IpldEmbed for bool {
  fn to_ipld(&self) -> Ipld { Ipld::Bool(*self) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Bool(x) => Ok(*x),
      xs => Err(IpldError::Expected(String::from("bool"), xs.clone())),
    }
  }
}

impl IpldEmbed for String {
  fn to_ipld(&self) -> Ipld { Ipld::String(self.clone()) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::String(s) => Ok(s.clone()),
      xs => Err(IpldError::Expected(String::from("String"), xs.clone())),
    }
  }
}

impl IpldEmbed for u64 {
  fn to_ipld(&self) -> Ipld { Ipld::Integer(*self as i128) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(x) => {
        let x = (*x).try_into().map_err(IpldError::U64)?;
        Ok(x)
      }
      xs => Err(IpldError::expected("u64", xs)),
    }
  }
}

impl IpldEmbed for BigUint {
  fn to_ipld(&self) -> Ipld { Ipld::Bytes(self.to_bytes_be()) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Bytes(bs) => Ok(BigUint::from_bytes_be(bs)),
      xs => Err(IpldError::Expected(String::from("BigUint"), xs.clone())),
    }
  }
}
impl<T: IpldEmbed> IpldEmbed for Vec<T> {
  fn to_ipld(&self) -> Ipld {
    Ipld::List(self.iter().map(|x| x.to_ipld()).collect())
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    let mut ys = Vec::new();
    match ipld {
      Ipld::List(xs) => {
        for x in xs {
          let y = T::from_ipld(x)?;
          ys.push(y);
        }
        Ok(ys)
      }
      xs => Err(IpldError::expected("List", xs)),
    }
  }
}

impl<T: IpldEmbed> IpldEmbed for Option<T> {
  fn to_ipld(&self) -> Ipld {
    match self {
      Some(x) => x.to_ipld(),
      None => Ipld::Null,
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Null => Ok(None),
      x => {
        let x = T::from_ipld(x)?;
        Ok(Some(x))
      }
    }
  }
}

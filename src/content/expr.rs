use crate::{
  content::{
    bind::{
      bind_from_ipld,
      bind_to_ipld,
    },
    cid::{
      ExprCid,
      NameCid,
      UnivCid,
    },
    ipld_error::IpldError,
  },
  expression::Bind,
};

use alloc::borrow::ToOwned;

use num_bigint::BigUint;
use sp_im::vector::Vector;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
  Ipld,
};
use sp_std::vec::Vec;

use sp_multihash::{
  Code,
  MultihashDigest,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Var { idx: BigUint },
  Sort { univ: UnivCid },
  Const { name: NameCid, levels: Vector<UnivCid> },
  App { fun: ExprCid, arg: ExprCid },
  Lam { info: Bind, name: NameCid, typ: ExprCid, bod: ExprCid },
  Pi { info: Bind, name: NameCid, typ: ExprCid, bod: ExprCid },
  Let { name: NameCid, typ: ExprCid, val: ExprCid, bod: ExprCid },
}

impl Expr {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var { idx } => Ipld::List(vec![
        Ipld::Integer(2),
        Ipld::Integer(0),
        Ipld::Bytes(idx.to_bytes_be()),
      ]),
      Self::Sort { univ } => {
        Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(1), Ipld::Link(univ.0)])
      }
      Self::Const { name, levels } => {
        let mut vec = Vec::new();
        for level in levels {
          vec.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::Integer(2),
          Ipld::Integer(2),
          Ipld::Link(name.0),
          Ipld::List(vec),
        ])
      }
      Self::App { fun, arg } => Ipld::List(vec![
        Ipld::Integer(2),
        Ipld::Integer(3),
        Ipld::Link(fun.0),
        Ipld::Link(arg.0),
      ]),
      Self::Lam { info, name, typ, bod } => Ipld::List(vec![
        Ipld::Integer(2),
        Ipld::Integer(4),
        bind_to_ipld(info),
        Ipld::Link(name.0),
        Ipld::Link(typ.0),
        Ipld::Link(bod.0),
      ]),
      Self::Pi { info, name, typ, bod } => Ipld::List(vec![
        Ipld::Integer(2),
        Ipld::Integer(5),
        bind_to_ipld(info),
        Ipld::Link(name.0),
        Ipld::Link(typ.0),
        Ipld::Link(bod.0),
      ]),
      Self::Let { name, typ, val, bod } => Ipld::List(vec![
        Ipld::Integer(2),
        Ipld::Integer(6),
        Ipld::Link(name.0),
        Ipld::Link(typ.0),
        Ipld::Link(val.0),
        Ipld::Link(bod.0),
      ]),
    }
  }

  pub fn cid(&self) -> ExprCid {
    ExprCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(2), Ipld::Integer(0), Ipld::Bytes(x)] => {
          Ok(Expr::Var { idx: BigUint::from_bytes_be(x) })
        }
        [Ipld::Integer(2), Ipld::Integer(1), Ipld::Link(x)] => {
          let univ = UnivCid::from_cid(*x).ok_or(IpldError::UnivCid(*x))?;
          Ok(Expr::Sort { univ })
        }
        [Ipld::Integer(2), Ipld::Integer(2), Ipld::Link(x), Ipld::List(ys)] => {
          let name = NameCid::from_cid(*x).ok_or(IpldError::NameCid(*x))?;
          let mut levels = Vec::new();
          for y in ys {
            match y {
              Ipld::Link(y) => {
                let level =
                  UnivCid::from_cid(*y).ok_or(IpldError::UnivCid(*x))?;
                levels.push(level);
              }
              _ => {
                return Err(IpldError::Univ(y.clone()));
              }
            }
          }
          Ok(Expr::Const { name, levels: levels.into() })
        }
        [Ipld::Integer(2), Ipld::Integer(3), Ipld::Link(x), Ipld::Link(y)] => {
          let fun = ExprCid::from_cid(*x).ok_or(IpldError::ExprCid(*x))?;
          let arg = ExprCid::from_cid(*y).ok_or(IpldError::ExprCid(*y))?;
          Ok(Expr::App { fun, arg })
        }
        [Ipld::Integer(2), Ipld::Integer(4), x, Ipld::Link(y), Ipld::Link(z), Ipld::Link(w)] =>
        {
          let info = bind_from_ipld(x)?;
          let name = NameCid::from_cid(*y).ok_or(IpldError::NameCid(*y))?;
          let typ = ExprCid::from_cid(*z).ok_or(IpldError::ExprCid(*z))?;
          let bod = ExprCid::from_cid(*w).ok_or(IpldError::ExprCid(*w))?;
          Ok(Expr::Lam { info, name, typ, bod })
        }
        [Ipld::Integer(2), Ipld::Integer(5), x, Ipld::Link(y), Ipld::Link(z), Ipld::Link(w)] =>
        {
          let info = bind_from_ipld(x)?;
          let name = NameCid::from_cid(*y).ok_or(IpldError::NameCid(*y))?;
          let typ = ExprCid::from_cid(*z).ok_or(IpldError::ExprCid(*z))?;
          let bod = ExprCid::from_cid(*w).ok_or(IpldError::ExprCid(*w))?;
          Ok(Expr::Pi { info, name, typ, bod })
        }
        [Ipld::Integer(2), Ipld::Integer(6), Ipld::Link(x), Ipld::Link(y), Ipld::Link(z), Ipld::Link(w)] =>
        {
          let name = NameCid::from_cid(*x).ok_or(IpldError::NameCid(*x))?;
          let typ = ExprCid::from_cid(*y).ok_or(IpldError::ExprCid(*y))?;
          let val = ExprCid::from_cid(*z).ok_or(IpldError::ExprCid(*z))?;
          let bod = ExprCid::from_cid(*w).ok_or(IpldError::ExprCid(*w))?;
          Ok(Expr::Let { name, typ, val, bod })
        }
        xs => Err(IpldError::Expr(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Expr(xs.to_owned())),
    }
  }
}

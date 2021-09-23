use crate::{
  expression::BinderInfo,
  format::{
    ipld_embed::{
      IpldEmbed,
      IpldError,
    },
    name::NameCid,
    univ::UnivCid,
    EXPR_CODEC,
  },
};

use alloc::{
  borrow::ToOwned,
  string::String,
};

use num_bigint::BigUint;
use sp_cid::{
  Cid,
  Version,
};
use sp_im::vector::Vector;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
  Ipld,
};
use sp_std::vec::Vec;

use sp_multihash::{
  Code,
  Multihash,
  MultihashDigest,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprCid {
  pub hash: Multihash,
}

impl IpldEmbed for ExprCid {
  fn cid(&self) -> Cid { Cid::new_v1(EXPR_CODEC, self.hash) }

  fn to_ipld(&self) -> Ipld { Ipld::Link(self.cid()) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Link(cid)
        if cid.codec() == EXPR_CODEC && cid.version() == Version::V1 =>
      {
        Ok(ExprCid { hash: *cid.hash() })
      }
      _ => Err(IpldError::ExpectedUnivCid(ipld.clone())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Var { idx: BigUint },
  Sort { univ: UnivCid },
  Const { name: NameCid, levels: Vector<UnivCid> },
  App { fun: ExprCid, arg: ExprCid },
  Lam { info: BinderInfo, name: NameCid, typ: ExprCid, bod: ExprCid },
  Pi { info: BinderInfo, name: NameCid, typ: ExprCid, bod: ExprCid },
  Let { name: NameCid, typ: ExprCid, val: ExprCid, bod: ExprCid },
}

pub fn bind_to_ipld(bind: &BinderInfo) -> Ipld {
  match bind {
    BinderInfo::Default => Ipld::Integer(0),
    BinderInfo::Implicit => Ipld::Integer(1),
    BinderInfo::StrictImplicit => Ipld::Integer(2),
    BinderInfo::InstImplicit => Ipld::Integer(3),
    BinderInfo::Rec => Ipld::Integer(4),
  }
}

pub fn bind_from_ipld(ipld: &Ipld) -> Result<BinderInfo, IpldError> {
  match ipld {
    Ipld::Integer(0) => Ok(BinderInfo::Default),
    Ipld::Integer(1) => Ok(BinderInfo::Implicit),
    Ipld::Integer(2) => Ok(BinderInfo::StrictImplicit),
    Ipld::Integer(3) => Ok(BinderInfo::InstImplicit),
    Ipld::Integer(4) => Ok(BinderInfo::Rec),
    _ => Err(IpldError::ExpectedBinderInfo(ipld.clone())),
  }
}

impl IpldEmbed for Expr {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var { idx } => Ipld::List(vec![
        Ipld::String(String::from("#EV")),
        Ipld::Bytes(idx.to_bytes_be()),
      ]),
      Self::Sort { univ } => {
        Ipld::List(vec![Ipld::String(String::from("#ES")), univ.to_ipld()])
      }
      Self::Const { name, levels } => {
        let mut vec = Vec::new();
        for level in levels {
          vec.push(level.to_ipld());
        }
        Ipld::List(vec![
          Ipld::String(String::from("#EC")),
          name.to_ipld(),
          Ipld::List(vec),
        ])
      }
      Self::App { fun, arg } => Ipld::List(vec![
        Ipld::String(String::from("#EA")),
        fun.to_ipld(),
        arg.to_ipld(),
      ]),
      Self::Lam { info, name, typ, bod } => Ipld::List(vec![
        Ipld::String(String::from("#EL")),
        bind_to_ipld(info),
        name.to_ipld(),
        typ.to_ipld(),
        bod.to_ipld(),
      ]),
      Self::Pi { info, name, typ, bod } => Ipld::List(vec![
        Ipld::String(String::from("#EP")),
        bind_to_ipld(info),
        name.to_ipld(),
        typ.to_ipld(),
        bod.to_ipld(),
      ]),
      Self::Let { name, typ, val, bod } => Ipld::List(vec![
        Ipld::String(String::from("#EZ")),
        name.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
        bod.to_ipld(),
      ]),
    }
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      EXPR_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), Bytes(i)] if tag == "#EV" => {
          Ok(Expr::Var { idx: BigUint::from_bytes_be(i) })
        }
        [String(tag), u] if tag == "#ES" => {
          let univ = UnivCid::from_ipld(u)?;
          Ok(Expr::Sort { univ })
        }
        [String(tag), n, List(ls)] if tag == "#EC" => {
          let name = NameCid::from_ipld(n)?;
          let mut levels = Vec::new();
          for l in ls {
            let level = UnivCid::from_ipld(l)?;
            levels.push(level);
          }
          Ok(Expr::Const { name, levels: levels.into() })
        }
        [String(tag), f, a] if tag == "#EA" => {
          let fun = ExprCid::from_ipld(f)?;
          let arg = ExprCid::from_ipld(a)?;
          Ok(Expr::App { fun, arg })
        }
        [String(tag), i, n, t, b] if tag == "#EL" => {
          let info = bind_from_ipld(i)?;
          let name = NameCid::from_ipld(n)?;
          let typ = ExprCid::from_ipld(t)?;
          let bod = ExprCid::from_ipld(b)?;
          Ok(Expr::Lam { info, name, typ, bod })
        }
        [String(tag), i, n, t, b] if tag == "#EP" => {
          let info = bind_from_ipld(i)?;
          let name = NameCid::from_ipld(n)?;
          let typ = ExprCid::from_ipld(t)?;
          let bod = ExprCid::from_ipld(b)?;
          Ok(Expr::Pi { info, name, typ, bod })
        }
        [String(tag), n, t, v, b] if tag == "#EZ" => {
          let name = NameCid::from_ipld(n)?;
          let typ = ExprCid::from_ipld(t)?;
          let val = ExprCid::from_ipld(v)?;
          let bod = ExprCid::from_ipld(b)?;
          Ok(Expr::Let { name, typ, val, bod })
        }
        xs => Err(IpldError::ExpectedExpr(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedExpr(xs.to_owned())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::{
    format::tests::arbitrary_cid,
    tests::{
      arbitrary_big_uint,
      frequency,
      gen_range,
    },
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;

  impl Arbitrary for ExprCid {
    fn arbitrary(g: &mut Gen) -> Self {
      let cid = arbitrary_cid(g, EXPR_CODEC);
      ExprCid { hash: *cid.hash() }
    }
  }

  impl Arbitrary for Expr {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Expr>)> = vec![
        (1, Box::new(|g| Expr::Var { idx: arbitrary_big_uint()(g) })),
        (1, Box::new(|g| Expr::Sort { univ: Arbitrary::arbitrary(g) })),
        (
          1,
          Box::new(|g| {
            let mut vec = Vec::new();
            let num = gen_range(g, 0..6);
            for _ in 0..num {
              vec.push(Arbitrary::arbitrary(g));
            }
            Expr::Const {
              name: Arbitrary::arbitrary(g),
              levels: Vector::from(vec),
            }
          }),
        ),
        (
          1,
          Box::new(|g| Expr::App {
            fun: Arbitrary::arbitrary(g),
            arg: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Expr::Lam {
            info: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            typ: Arbitrary::arbitrary(g),
            bod: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Expr::Pi {
            info: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            typ: Arbitrary::arbitrary(g),
            bod: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Expr::Let {
            name: Arbitrary::arbitrary(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
            bod: Arbitrary::arbitrary(g),
          }),
        ),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn expr_ipld(x: Expr) -> bool {
    match Expr::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

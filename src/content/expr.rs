use crate::{
  content::{
    cid::{
      ConstCid,
      ExprCid,
      UnivCid,
    },
    ipld_error::IpldError,
  },
  expression::{
    BinderInfo,
    Literal,
  },
};

use num_bigint::BigUint;

use alloc::{
  borrow::ToOwned,
  string::String,
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
  MultihashDigest,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Var { idx: BigUint },
  Sort { univ: UnivCid },
  Const { constant: ConstCid, levels: Vector<UnivCid> },
  App { fun: ExprCid, arg: ExprCid },
  Lam { info: BinderInfo, typ: ExprCid, bod: ExprCid },
  Pi { info: BinderInfo, typ: ExprCid, bod: ExprCid },
  Let { typ: ExprCid, val: ExprCid, bod: ExprCid },
  Lit { val: Literal },
}

impl Literal {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Literal::Nat(x) => Ipld::List(vec![
        Ipld::Integer(6),
        Ipld::Integer(0),
        Ipld::Bytes(x.to_bytes_be()),
      ]),
      Literal::Str(x) => Ipld::List(vec![
        Ipld::Integer(6),
        Ipld::Integer(1),
        Ipld::String(x.clone()),
      ]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(6), Integer(0), Bytes(i)] => {
          Ok(Literal::Nat(BigUint::from_bytes_be(i)))
        }
        [Integer(6), Integer(1), String(s)] => Ok(Literal::Str(s.to_owned())),
        xs => Err(IpldError::Expr(List(xs.to_owned()))),
      },
      x => Err(IpldError::Literal(x.clone())),
    }
  }
}

impl BinderInfo {
  pub fn to_ipld(self) -> Ipld {
    match self {
      BinderInfo::Default => Ipld::Integer(0),
      BinderInfo::Implicit => Ipld::Integer(1),
      BinderInfo::StrictImplicit => Ipld::Integer(2),
      BinderInfo::InstImplicit => Ipld::Integer(3),
      BinderInfo::AuxDecl => Ipld::Integer(4),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(BinderInfo::Default),
      Ipld::Integer(1) => Ok(BinderInfo::Implicit),
      Ipld::Integer(2) => Ok(BinderInfo::StrictImplicit),
      Ipld::Integer(3) => Ok(BinderInfo::InstImplicit),
      Ipld::Integer(4) => Ok(BinderInfo::AuxDecl),
      _ => Err(IpldError::BinderInfo(ipld.clone())),
    }
  }
}

impl Expr {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var { idx } => Ipld::List(vec![
        Ipld::Integer(4),
        Ipld::Integer(0),
        Ipld::Bytes(idx.to_bytes_be()),
      ]),
      Self::Sort { univ } => {
        Ipld::List(vec![Ipld::Integer(4), Ipld::Integer(1), Ipld::Link(univ.0)])
      }
      Self::Const { constant, levels } => {
        let mut vec = Vec::new();
        for level in levels {
          vec.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::Integer(4),
          Ipld::Integer(2),
          Ipld::Link(constant.0),
          Ipld::List(vec),
        ])
      }
      Self::App { fun, arg } => Ipld::List(vec![
        Ipld::Integer(4),
        Ipld::Integer(3),
        Ipld::Link(fun.0),
        Ipld::Link(arg.0),
      ]),
      Self::Lam { info, typ, bod } => Ipld::List(vec![
        Ipld::Integer(4),
        Ipld::Integer(4),
        info.to_ipld(),
        Ipld::Link(typ.0),
        Ipld::Link(bod.0),
      ]),
      Self::Pi { info, typ, bod } => Ipld::List(vec![
        Ipld::Integer(4),
        Ipld::Integer(5),
        info.to_ipld(),
        Ipld::Link(typ.0),
        Ipld::Link(bod.0),
      ]),
      Self::Let { typ, val, bod } => Ipld::List(vec![
        Ipld::Integer(4),
        Ipld::Integer(6),
        Ipld::Link(typ.0),
        Ipld::Link(val.0),
        Ipld::Link(bod.0),
      ]),
      Self::Lit { val } => {
        Ipld::List(vec![Ipld::Integer(4), Ipld::Integer(7), val.to_ipld()])
      }
    }
  }

  pub fn cid(&self) -> ExprCid {
    ExprCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(4), Integer(0), Bytes(i)] => {
          Ok(Expr::Var { idx: BigUint::from_bytes_be(i) })
        }
        [Integer(4), Integer(1), Link(u)] => {
          let univ = UnivCid::from_cid(*u)?;
          Ok(Expr::Sort { univ })
        }
        [Integer(4), Integer(2), Link(n), List(ls)] => {
          let constant = ConstCid::from_cid(*n)?;
          let mut levels = Vec::new();
          for l in ls {
            match l {
              Link(l) => {
                let level = UnivCid::from_cid(*l)?;
                levels.push(level);
              }
              _ => {
                return Err(IpldError::Univ(l.clone()));
              }
            }
          }
          Ok(Expr::Const { constant, levels: levels.into() })
        }
        [Integer(4), Integer(3), Link(f), Link(a)] => {
          let fun = ExprCid::from_cid(*f)?;
          let arg = ExprCid::from_cid(*a)?;
          Ok(Expr::App { fun, arg })
        }
        [Integer(4), Integer(4), i, Link(t), Link(b)] => {
          let info = BinderInfo::from_ipld(i)?;
          let typ = ExprCid::from_cid(*t)?;
          let bod = ExprCid::from_cid(*b)?;
          Ok(Expr::Lam { info, typ, bod })
        }
        [Integer(4), Integer(5), i, Link(t), Link(b)] => {
          let info = BinderInfo::from_ipld(i)?;
          let typ = ExprCid::from_cid(*t)?;
          let bod = ExprCid::from_cid(*b)?;
          Ok(Expr::Pi { info, typ, bod })
        }
        [Integer(4), Integer(6), Link(t), Link(v), Link(b)] => {
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          let bod = ExprCid::from_cid(*b)?;
          Ok(Expr::Let { typ, val, bod })
        }
        [Integer(4), Integer(7), lit] => {
          let lit = Literal::from_ipld(lit)?;
          Ok(Expr::Lit { val: lit })
        }
        xs => Err(IpldError::Expr(List(xs.to_owned()))),
      },
      xs => Err(IpldError::Expr(xs.to_owned())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::{
    content::tests::frequency,
    tests::{
      arbitrary_big_uint,
      gen_range,
    },
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

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
              constant: Arbitrary::arbitrary(g),
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
            typ: Arbitrary::arbitrary(g),
            bod: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Expr::Pi {
            info: Arbitrary::arbitrary(g),
            typ: Arbitrary::arbitrary(g),
            bod: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Expr::Let {
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

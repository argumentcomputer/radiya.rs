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

use alloc::{
  borrow::ToOwned,
  string::String,
};

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
        Ipld::String(String::from("#EV")),
        Ipld::Bytes(idx.to_bytes_be()),
      ]),
      Self::Sort { univ } => {
        Ipld::List(vec![Ipld::String(String::from("#ES")), Ipld::Link(univ.0)])
      }
      Self::Const { name, levels } => {
        let mut vec = Vec::new();
        for level in levels {
          vec.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::String(String::from("#EC")),
          Ipld::Link(name.0),
          Ipld::List(vec),
        ])
      }
      Self::App { fun, arg } => Ipld::List(vec![
        Ipld::String(String::from("#EA")),
        Ipld::Link(fun.0),
        Ipld::Link(arg.0),
      ]),
      Self::Lam { info, name, typ, bod } => Ipld::List(vec![
        Ipld::String(String::from("#EL")),
        bind_to_ipld(info),
        Ipld::Link(name.0),
        Ipld::Link(typ.0),
        Ipld::Link(bod.0),
      ]),
      Self::Pi { info, name, typ, bod } => Ipld::List(vec![
        Ipld::String(String::from("#EP")),
        bind_to_ipld(info),
        Ipld::Link(name.0),
        Ipld::Link(typ.0),
        Ipld::Link(bod.0),
      ]),
      Self::Let { name, typ, val, bod } => Ipld::List(vec![
        Ipld::String(String::from("#EZ")),
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

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), Bytes(i)] if tag == "#EV" => {
          Ok(Expr::Var { idx: BigUint::from_bytes_be(i) })
        }
        [String(tag), Link(u)] if tag == "#ES" => {
          let univ = UnivCid::from_cid(*u)?;
          Ok(Expr::Sort { univ })
        }
        [String(tag), Link(n), List(ls)] if tag == "#EC" => {
          let name = NameCid::from_cid(*n)?;
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
          Ok(Expr::Const { name, levels: levels.into() })
        }
        [String(tag), Link(f), Link(a)] if tag == "#EA" => {
          let fun = ExprCid::from_cid(*f)?;
          let arg = ExprCid::from_cid(*a)?;
          Ok(Expr::App { fun, arg })
        }
        [String(tag), i, Link(n), Link(t), Link(b)] if tag == "#EL" => {
          let info = bind_from_ipld(i)?;
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let bod = ExprCid::from_cid(*b)?;
          Ok(Expr::Lam { info, name, typ, bod })
        }
        [String(tag), i, Link(n), Link(t), Link(b)] if tag == "#EP" => {
          let info = bind_from_ipld(i)?;
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let bod = ExprCid::from_cid(*b)?;
          Ok(Expr::Pi { info, name, typ, bod })
        }
        [String(tag), Link(n), Link(t), Link(v), Link(b)] if tag == "#EZ" => {
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          let bod = ExprCid::from_cid(*b)?;
          Ok(Expr::Let { name, typ, val, bod })
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

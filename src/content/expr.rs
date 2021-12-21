use crate::{
  content::{
    cid::{
      ConstCid,
      ConstMetaCid,
      ExprCid,
      ExprMetaCid,
      LitCid,
      NameCid,
      UnivCid,
      UnivMetaCid,
      EXPR,
      LITERAL,
    },
    ipld::{
      IpldEmbed,
      IpldError,
    },
  },
  expression::{
    BinderInfo,
    Literal,
  },
  parse::position::Pos,
};

use num_bigint::BigUint;

use alloc::{
  borrow::ToOwned,
  string::String,
};

use sp_im::vector::Vector;
use sp_ipld::Ipld;
use sp_std::vec::Vec;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Var { idx: BigUint },
  Sort { univ: UnivCid },
  Const { constant: ConstCid, levels: Vector<UnivCid> },
  App { fun: ExprCid, arg: ExprCid },
  Lam { info: BinderInfo, typ: ExprCid, bod: ExprCid },
  Pi { info: BinderInfo, typ: ExprCid, bod: ExprCid },
  Let { typ: ExprCid, val: ExprCid, bod: ExprCid },
  Lit { val: LitCid },
  Fix { bod: ExprCid },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprMeta {
  Var(Pos, NameCid),
  Sort(Pos, UnivMetaCid),
  Const(Pos, NameCid, ConstMetaCid, Vec<UnivMetaCid>),
  App(Pos, ExprMetaCid, ExprMetaCid),
  Lam(Pos, NameCid, ExprMetaCid, ExprMetaCid),
  Pi(Pos, NameCid, ExprMetaCid, ExprMetaCid),
  Let(Pos, NameCid, ExprMetaCid, ExprMetaCid, ExprMetaCid),
  Lit(Pos),
  Fix(Pos, NameCid, ExprMetaCid),
}

impl IpldEmbed for BinderInfo {
  fn to_ipld(&self) -> Ipld {
    match self {
      BinderInfo::Default => Ipld::Integer(0),
      BinderInfo::Implicit => Ipld::Integer(1),
      BinderInfo::StrictImplicit => Ipld::Integer(2),
      BinderInfo::InstImplicit => Ipld::Integer(3),
      BinderInfo::AuxDecl => Ipld::Integer(4),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(BinderInfo::Default),
      Ipld::Integer(1) => Ok(BinderInfo::Implicit),
      Ipld::Integer(2) => Ok(BinderInfo::StrictImplicit),
      Ipld::Integer(3) => Ok(BinderInfo::InstImplicit),
      Ipld::Integer(4) => Ok(BinderInfo::AuxDecl),
      _ => Err(IpldError::Expected(String::from("BinderInfo"), ipld.clone())),
    }
  }
}

impl IpldEmbed for Literal {
  fn to_ipld(&self) -> Ipld {
    match self {
      Literal::Nat(x) => Ipld::List(vec![
        Ipld::Integer(LITERAL.into()),
        Ipld::Integer(0),
        x.to_ipld(),
      ]),
      Literal::Str(x) => Ipld::List(vec![
        Ipld::Integer(LITERAL.into()),
        Ipld::Integer(1),
        x.to_ipld(),
      ]),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = LITERAL.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0), Bytes(i)] if *t == tag => {
          Ok(Literal::Nat(BigUint::from_bytes_be(i)))
        }
        [Integer(t), Integer(1), String(s)] if *t == tag => {
          Ok(Literal::Str(s.to_owned()))
        }
        xs => Err(IpldError::expected("Expression", &List(xs.to_owned()))),
      },
      x => Err(IpldError::expected("Expression", x)),
    }
  }
}

impl Expr {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var { idx } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(0),
        idx.to_ipld(),
      ]),
      Self::Sort { univ } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(1),
        univ.to_ipld(),
      ]),
      Self::Const { constant, levels } => {
        let mut vec = Vec::new();
        for level in levels {
          vec.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::Integer(EXPR.into()),
          Ipld::Integer(2),
          constant.to_ipld(),
          Ipld::List(vec),
        ])
      }
      Self::App { fun, arg } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(3),
        fun.to_ipld(),
        arg.to_ipld(),
      ]),
      Self::Lam { info, typ, bod } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(4),
        info.to_ipld(),
        typ.to_ipld(),
        bod.to_ipld(),
      ]),
      Self::Pi { info, typ, bod } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(5),
        info.to_ipld(),
        typ.to_ipld(),
        bod.to_ipld(),
      ]),
      Self::Let { typ, val, bod } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(6),
        typ.to_ipld(),
        val.to_ipld(),
        bod.to_ipld(),
      ]),
      Self::Lit { val } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(7),
        val.to_ipld(),
      ]),
      Self::Fix { bod } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(8),
        bod.to_ipld(),
      ]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = EXPR.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0), idx] if *t == tag => {
          let idx = BigUint::from_ipld(idx)?;
          Ok(Expr::Var { idx })
        }
        [Integer(t), Integer(1), univ] if *t == tag => {
          let univ = UnivCid::from_ipld(univ)?;
          Ok(Expr::Sort { univ })
        }
        [Integer(t), Integer(2), con, List(ls)] if *t == tag => {
          let constant = ConstCid::from_ipld(con)?;
          let mut levels = Vec::new();
          for l in ls {
            let level = UnivCid::from_ipld(l)?;
            levels.push(level);
          }
          Ok(Expr::Const { constant, levels: levels.into() })
        }
        [Integer(t), Integer(3), fun, arg] if *t == tag => {
          let fun = ExprCid::from_ipld(fun)?;
          let arg = ExprCid::from_ipld(arg)?;
          Ok(Expr::App { fun, arg })
        }
        [Integer(t), Integer(4), info, typ, bod] if *t == tag => {
          let info = BinderInfo::from_ipld(info)?;
          let typ = ExprCid::from_ipld(typ)?;
          let bod = ExprCid::from_ipld(bod)?;
          Ok(Expr::Lam { info, typ, bod })
        }
        [Integer(t), Integer(5), info, typ, bod] if *t == tag => {
          let info = BinderInfo::from_ipld(info)?;
          let typ = ExprCid::from_ipld(typ)?;
          let bod = ExprCid::from_ipld(bod)?;
          Ok(Expr::Pi { info, typ, bod })
        }
        [Integer(t), Integer(6), typ, val, bod] if *t == tag => {
          let typ = ExprCid::from_ipld(typ)?;
          let val = ExprCid::from_ipld(val)?;
          let bod = ExprCid::from_ipld(bod)?;
          Ok(Expr::Let { typ, val, bod })
        }
        [Integer(t), Integer(7), lit] if *t == tag => {
          let val = LitCid::from_ipld(lit)?;
          Ok(Expr::Lit { val })
        }
        [Integer(t), Integer(8), bod] if *t == tag => {
          let bod = ExprCid::from_ipld(bod)?;
          Ok(Expr::Fix { bod })
        }
        xs => Err(IpldError::expected("Expression", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("Expression", xs)),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::tests::{
    arbitrary_big_uint,
    frequency,
    gen_range,
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

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
      EXPR_META,
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
use sp_std::{
  fmt,
  fmt::Display,
  vec::Vec,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Var { idx: BigUint },
  Sort { univ: UnivCid },
  Const { constant: ConstCid, levels: Vec<UnivCid> },
  App { fun: ExprCid, arg: ExprCid },
  Lam { info: BinderInfo, typ: ExprCid, bod: ExprCid },
  Pi { info: BinderInfo, typ: ExprCid, bod: ExprCid },
  Let { typ: ExprCid, val: ExprCid, bod: ExprCid },
  Lit { val: LitCid },
  Fix { bod: ExprCid },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprMeta {
  Var(Pos),
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
        xs => Err(IpldError::expected("Expr", &List(xs.to_owned()))),
      },
      x => Err(IpldError::expected("Expr", x)),
    }
  }
}

impl Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Var { idx } => {
        write!(f, "Var {}", idx)
      }
      Self::Sort { univ } => {
        write!(f, "Sort {}", univ)
      }
      Self::Const { constant, levels } => {
        write!(f, "Const {} ", constant)?;
        for l in levels {
          write!(f, "{}", l)?;
        }
        Ok(())
      }
      Self::App { fun, arg } => {
        write!(f, "App {} {}", fun, arg)
      }
      Self::Lam { info, typ, bod } => {
        write!(f, "Lam {} {} {}", info, typ, bod)
      }
      Self::Pi { info, typ, bod } => {
        write!(f, "Pi {} {} {}", info, typ, bod)
      }
      Self::Let { typ, val, bod } => {
        write!(f, "Let {} {} {}", typ, val, bod)
      }
      Self::Lit { val } => {
        write!(f, "Lit {}", val)
      }
      Self::Fix { bod } => {
        write!(f, "Fix {}", bod)
      }
    }
  }
}

impl IpldEmbed for Expr {
  fn to_ipld(&self) -> Ipld {
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
      Self::Const { constant, levels } => Ipld::List(vec![
        Ipld::Integer(EXPR.into()),
        Ipld::Integer(2),
        constant.to_ipld(),
        levels.to_ipld(),
      ]),
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

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
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
        [Integer(t), Integer(2), constant, levels] if *t == tag => {
          let constant = ConstCid::from_ipld(constant)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          Ok(Expr::Const { constant, levels })
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

impl Display for ExprMeta {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let print_pos = |x| match x {
      Some(pos) => format!("{}", pos),
      None => format!("_"),
    };
    match self {
      Self::Var(pos) => {
        write!(f, "Var {}", print_pos(*pos))
      }
      Self::Sort(pos, cid) => {
        write!(f, "Sort {} {}", print_pos(*pos), cid)
      }
      Self::Const(pos, name, typ, levels) => {
        write!(f, "Const {} {} {}", print_pos(*pos), name, typ)?;
        for l in levels {
          write!(f, "{}", l)?;
        }
        Ok(())
      }
      Self::App(pos, fun, arg) => {
        write!(f, "App {} {} {}", print_pos(*pos), fun, arg)
      }
      Self::Lam(pos, nam, typ, bod) => {
        write!(f, "Lam {} {} {} {}", print_pos(*pos), nam, typ, bod)
      }
      Self::Pi(pos, nam, typ, bod) => {
        write!(f, "Pi {} {} {} {}", print_pos(*pos), nam, typ, bod)
      }
      Self::Let(pos, nam, typ, val, bod) => {
        write!(f, "Let {} {} {} {} {}", print_pos(*pos), nam, typ, val, bod)
      }
      Self::Lit(pos) => {
        write!(f, "Lit {}", print_pos(*pos))
      }
      Self::Fix(pos, nam, bod) => {
        write!(f, "Fix {} {} {}", print_pos(*pos), nam, bod)
      }
    }
  }
}

impl IpldEmbed for ExprMeta {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var(pos) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(0),
        pos.to_ipld(),
      ]),
      Self::Sort(pos, univ_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(1),
        pos.to_ipld(),
        univ_meta.to_ipld(),
      ]),
      Self::Const(pos, name, constant_meta, levels_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(2),
        pos.to_ipld(),
        name.to_ipld(),
        constant_meta.to_ipld(),
        levels_meta.to_ipld(),
      ]),
      Self::App(pos, fun_meta, arg_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(3),
        pos.to_ipld(),
        fun_meta.to_ipld(),
        arg_meta.to_ipld(),
      ]),
      Self::Lam(pos, name, typ_meta, bod_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(4),
        pos.to_ipld(),
        name.to_ipld(),
        typ_meta.to_ipld(),
        bod_meta.to_ipld(),
      ]),
      Self::Pi(pos, name, typ_meta, bod_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(5),
        pos.to_ipld(),
        name.to_ipld(),
        typ_meta.to_ipld(),
        bod_meta.to_ipld(),
      ]),
      Self::Let(pos, name, typ_meta, val_meta, bod_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(6),
        pos.to_ipld(),
        name.to_ipld(),
        typ_meta.to_ipld(),
        val_meta.to_ipld(),
        bod_meta.to_ipld(),
      ]),
      Self::Lit(pos) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(7),
        pos.to_ipld(),
      ]),
      Self::Fix(pos, name, bod_meta) => Ipld::List(vec![
        Ipld::Integer(EXPR_META.into()),
        Ipld::Integer(8),
        pos.to_ipld(),
        name.to_ipld(),
        bod_meta.to_ipld(),
      ]),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = EXPR_META.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0), pos] if *t == tag => {
          let pos = IpldEmbed::from_ipld(pos)?;
          Ok(ExprMeta::Var(pos))
        }
        [Integer(t), Integer(1), pos, univ_meta] if *t == tag => {
          let pos = IpldEmbed::from_ipld(pos)?;
          let univ_meta = UnivMetaCid::from_ipld(univ_meta)?;
          Ok(ExprMeta::Sort(pos, univ_meta))
        }
        [Integer(t), Integer(2), pos, name, constant_meta, levels_meta]
          if *t == tag =>
        {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let constant_meta = ConstMetaCid::from_ipld(constant_meta)?;
          let levels_meta = IpldEmbed::from_ipld(levels_meta)?;
          Ok(ExprMeta::Const(pos, name, constant_meta, levels_meta))
        }
        [Integer(t), Integer(3), pos, fun_meta, arg_meta] if *t == tag => {
          let pos = IpldEmbed::from_ipld(pos)?;
          let fun_meta = ExprMetaCid::from_ipld(fun_meta)?;
          let arg_meta = ExprMetaCid::from_ipld(arg_meta)?;
          Ok(ExprMeta::App(pos, fun_meta, arg_meta))
        }
        [Integer(t), Integer(4), pos, name, typ_meta, bod_meta]
          if *t == tag =>
        {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let typ_meta = ExprMetaCid::from_ipld(typ_meta)?;
          let bod_meta = ExprMetaCid::from_ipld(bod_meta)?;
          Ok(ExprMeta::Lam(pos, name, typ_meta, bod_meta))
        }
        [Integer(t), Integer(5), pos, name, typ_meta, bod_meta]
          if *t == tag =>
        {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let typ_meta = ExprMetaCid::from_ipld(typ_meta)?;
          let bod_meta = ExprMetaCid::from_ipld(bod_meta)?;
          Ok(ExprMeta::Pi(pos, name, typ_meta, bod_meta))
        }
        [Integer(t), Integer(6), pos, name, typ_meta, val_meta, bod_meta]
          if *t == tag =>
        {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let typ_meta = ExprMetaCid::from_ipld(typ_meta)?;
          let val_meta = ExprMetaCid::from_ipld(val_meta)?;
          let bod_meta = ExprMetaCid::from_ipld(bod_meta)?;
          Ok(ExprMeta::Let(pos, name, typ_meta, val_meta, bod_meta))
        }
        [Integer(t), Integer(7), pos] if *t == tag => {
          let pos = IpldEmbed::from_ipld(pos)?;
          Ok(ExprMeta::Lit(pos))
        }
        [Integer(t), Integer(8), pos, name, bod_meta] if *t == tag => {
          let pos = IpldEmbed::from_ipld(pos)?;
          let name = IpldEmbed::from_ipld(name)?;
          let bod_meta = ExprMetaCid::from_ipld(bod_meta)?;
          Ok(ExprMeta::Fix(pos, name, bod_meta))
        }
        xs => Err(IpldError::expected("ExprMeta", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("ExprMeta", xs)),
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
            Expr::Const { constant: Arbitrary::arbitrary(g), levels: vec }
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
        (1, Box::new(|g| Self::Fix { bod: Arbitrary::arbitrary(g) })),
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

  impl Arbitrary for ExprMeta {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Self>)> = vec![
        (1, Box::new(|g| Self::Var(Arbitrary::arbitrary(g)))),
        (
          1,
          Box::new(|g| {
            Self::Sort(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g))
          }),
        ),
        (
          1,
          Box::new(|g| {
            let mut vec = Vec::new();
            let num = gen_range(g, 0..6);
            for _ in 0..num {
              vec.push(Arbitrary::arbitrary(g));
            }
            Self::Const(
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              vec,
            )
          }),
        ),
        (
          1,
          Box::new(|g| {
            Self::App(
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
            )
          }),
        ),
        (
          1,
          Box::new(|g| {
            Self::Lam(
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
            )
          }),
        ),
        (
          1,
          Box::new(|g| {
            Self::Pi(
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
            )
          }),
        ),
        (
          1,
          Box::new(|g| {
            Self::Let(
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
            )
          }),
        ),
        (
          1,
          Box::new(|g| {
            Self::Fix(
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
              Arbitrary::arbitrary(g),
            )
          }),
        ),
      ];
      frequency(g, input)
    }
  }
  #[quickcheck]
  fn expr_meta_ipld(x: ExprMeta) -> bool {
    match ExprMeta::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

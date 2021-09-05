use crate::{
  content::{
    cid::{
      DeclCid,
      ExprCid,
      NameCid,
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
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
  Definition {
    name: NameCid,
    typ: ExprCid,
    val: ExprCid,
    levels: Vector<NameCid>,
    is_unsafe: bool,
  },
  Axiom {
    name: NameCid,
    typ: ExprCid,
    levels: Vector<NameCid>,
    is_unsafe: bool,
  },
  Inductive {
    num_params: BigUint,
    name: NameCid,
    typ: ExprCid,
    intros: Vector<(NameCid, ExprCid)>,
    levels: Vector<NameCid>,
    is_unsafe: bool,
  },
  Theorem {
    name: NameCid,
    typ: ExprCid,
    val: ExprCid,
    levels: Vector<NameCid>,
  },
  Opaque {
    name: NameCid,
    typ: ExprCid,
    val: ExprCid,
    levels: Vector<NameCid>,
  },
  Quotient,
  Notation {
    fixity: Fixity,
    name: NameCid,
    prec: BigUint,
    token: String,
  },
}

impl Decl {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Quotient => Ipld::List(vec![Ipld::String(String::from("#QUOT"))]),
      Self::Notation { fixity, name, prec, token } => {
        let tag = match fixity {
          Fixity::Infix => "#INFIX",
          Fixity::Prefix => "#PREFIX",
          Fixity::Postfix => "#POSTFIX",
        };
        Ipld::List(vec![
          Ipld::String(String::from(tag)),
          Ipld::Link(name.0),
          Ipld::Bytes(prec.to_bytes_be()),
          Ipld::String(token.clone()),
        ])
      }
      Self::Definition { name, typ, val, levels, is_unsafe } => {
        let mut ls = Vec::new();
        for level in levels {
          ls.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::String(String::from("#DEF")),
          Ipld::Link(name.0),
          Ipld::Link(typ.0),
          Ipld::Link(val.0),
          Ipld::List(ls),
          Ipld::Bool(*is_unsafe),
        ])
      }
      Self::Axiom { name, typ, levels, is_unsafe } => {
        let mut ls = Vec::new();
        for level in levels {
          ls.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::String(String::from("#AX")),
          Ipld::Link(name.0),
          Ipld::Link(typ.0),
          Ipld::List(ls),
          Ipld::Bool(*is_unsafe),
        ])
      }
      Self::Theorem { name, typ, val, levels } => {
        let mut ls = Vec::new();
        for level in levels {
          ls.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::String(String::from("#THEO")),
          Ipld::Link(name.0),
          Ipld::Link(typ.0),
          Ipld::Link(val.0),
          Ipld::List(ls),
        ])
      }
      Self::Opaque { name, typ, val, levels } => {
        let mut ls = Vec::new();
        for level in levels {
          ls.push(Ipld::Link(level.0));
        }
        Ipld::List(vec![
          Ipld::String(String::from("#OPAQ")),
          Ipld::Link(name.0),
          Ipld::Link(typ.0),
          Ipld::Link(val.0),
          Ipld::List(ls),
        ])
      }
      Self::Inductive { num_params, name, typ, intros, levels, is_unsafe } => {
        let mut ls = Vec::new();
        let mut is = Vec::new();
        for level in levels {
          ls.push(Ipld::Link(level.0));
        }
        for (n, x) in intros {
          is.push(Ipld::List(vec![Ipld::Link(n.0), Ipld::Link(x.0)]));
        }
        Ipld::List(vec![
          Ipld::String(String::from("#IND")),
          Ipld::Bytes(num_params.to_bytes_be()),
          Ipld::Link(name.0),
          Ipld::Link(typ.0),
          Ipld::List(is),
          Ipld::List(ls),
          Ipld::Bool(*is_unsafe),
        ])
      }
    }
  }

  pub fn cid(&self) -> DeclCid {
    DeclCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag)] if tag == "#QUOT" => Ok(Decl::Quotient),
        [String(tag), Link(x), Bytes(y), String(z)]
          if tag == "#INFIX" || tag == "#PREFIX" || tag == "#POSTFIX" =>
        {
          let fixity = match tag.as_str() {
            "#INFIX" => Fixity::Infix,
            "#PREFIX" => Fixity::Prefix,
            "#POSTFIX" => Fixity::Postfix,
            _ => unreachable!(),
          };
          let name = NameCid::from_cid(*x)?;
          Ok(Decl::Notation {
            fixity,
            name,
            prec: BigUint::from_bytes_be(y),
            token: z.clone(),
          })
        }
        [String(tag), Link(n), Link(t), Link(v), List(ls), Bool(u)]
          if tag == "#DEF" =>
        {
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          let mut levels = Vec::new();
          for l in ls {
            match l {
              Link(l) => {
                let l = NameCid::from_cid(*l)?;
                levels.push(l);
              }
              _ => return Err(IpldError::Name(l.clone())),
            }
          }
          Ok(Decl::Definition {
            name,
            typ,
            val,
            levels: Vector::from(levels),
            is_unsafe: *u,
          })
        }
        [String(tag), Bytes(np), Link(n), Link(t), List(is), List(ls), Bool(u)]
          if tag == "#IND" =>
        {
          let num_params = BigUint::from_bytes_be(np);
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let mut levels = Vec::new();
          for l in ls {
            match l {
              Link(l) => {
                let l = NameCid::from_cid(*l)?;
                levels.push(l);
              }
              _ => return Err(IpldError::Name(l.clone())),
            }
          }
          let mut intros = Vec::new();
          for i in is {
            match i {
              List(i) => match i.as_slice() {
                [Link(n), Link(x)] => {
                  let n = NameCid::from_cid(*n)?;
                  let x = ExprCid::from_cid(*x)?;
                  intros.push((n, x));
                }
                _ => return Err(IpldError::Intro(List(i.clone()))),
              },
              _ => return Err(IpldError::Intro(i.clone())),
            }
          }
          Ok(Decl::Inductive {
            num_params,
            name,
            typ,
            intros: Vector::from(intros),
            levels: Vector::from(levels),
            is_unsafe: *u,
          })
        }
        [String(tag), Link(n), Link(t), Ipld::List(ls), Bool(u)]
          if tag == "#AX" =>
        {
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let mut levels = Vec::new();
          for l in ls {
            match l {
              Link(l) => {
                let l = NameCid::from_cid(*l)?;
                levels.push(l);
              }
              _ => return Err(IpldError::Name(l.clone())),
            }
          }
          Ok(Decl::Axiom {
            name,
            typ,
            levels: Vector::from(levels),
            is_unsafe: *u,
          })
        }
        [String(tag), Link(n), Link(t), Link(v), Ipld::List(ls)]
          if tag == "#THEO" =>
        {
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          let mut levels = Vec::new();
          for l in ls {
            match l {
              Link(l) => {
                let l = NameCid::from_cid(*l)?;
                levels.push(l);
              }
              _ => return Err(IpldError::Name(l.clone())),
            }
          }
          Ok(Decl::Theorem { name, typ, val, levels: Vector::from(levels) })
        }
        [String(tag), Link(n), Link(t), Link(v), Ipld::List(ls)]
          if tag == "#OPAQ" =>
        {
          let name = NameCid::from_cid(*n)?;
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          let mut levels = Vec::new();
          for l in ls {
            match l {
              Link(l) => {
                let l = NameCid::from_cid(*l)?;
                levels.push(l);
              }
              _ => return Err(IpldError::Name(l.clone())),
            }
          }
          Ok(Decl::Opaque { name, typ, val, levels: Vector::from(levels) })
        }
        xs => Err(IpldError::Decl(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Decl(xs.to_owned())),
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

  impl Arbitrary for Decl {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Decl>)> = vec![
        (1, Box::new(|_| Decl::Quotient)),
        (
          1,
          Box::new(|g| {
            let x: usize = Arbitrary::arbitrary(g);
            let fixity = match x % 3 {
              0 => Fixity::Infix,
              1 => Fixity::Prefix,
              2 => Fixity::Postfix,
              _ => unreachable!(),
            };
            Decl::Notation {
              fixity,
              token: Arbitrary::arbitrary(g),
              prec: arbitrary_big_uint()(g),
              name: Arbitrary::arbitrary(g),
            }
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
            Decl::Theorem {
              name: Arbitrary::arbitrary(g),
              typ: Arbitrary::arbitrary(g),
              val: Arbitrary::arbitrary(g),
              levels: Vector::from(vec),
            }
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
            Decl::Opaque {
              name: Arbitrary::arbitrary(g),
              typ: Arbitrary::arbitrary(g),
              val: Arbitrary::arbitrary(g),
              levels: Vector::from(vec),
            }
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
            Decl::Definition {
              name: Arbitrary::arbitrary(g),
              typ: Arbitrary::arbitrary(g),
              val: Arbitrary::arbitrary(g),
              levels: Vector::from(vec),
              is_unsafe: Arbitrary::arbitrary(g),
            }
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
            Decl::Axiom {
              name: Arbitrary::arbitrary(g),
              typ: Arbitrary::arbitrary(g),
              levels: Vector::from(vec),
              is_unsafe: Arbitrary::arbitrary(g),
            }
          }),
        ),
        (
          1,
          Box::new(|g| {
            let mut intros = Vec::new();
            let mut levels = Vec::new();
            let num = gen_range(g, 0..6);
            for _ in 0..num {
              levels.push(Arbitrary::arbitrary(g));
              intros.push((Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)));
            }
            Decl::Inductive {
              num_params: levels.len().into(),
              name: Arbitrary::arbitrary(g),
              typ: Arbitrary::arbitrary(g),
              levels: Vector::from(levels),
              intros: Vector::from(intros),
              is_unsafe: Arbitrary::arbitrary(g),
            }
          }),
        ),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn decl_ipld(x: Decl) -> bool {
    match Decl::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

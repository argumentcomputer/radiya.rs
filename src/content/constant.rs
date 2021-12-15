use crate::content::{
  cid::{
    ConstCid,
    ExprCid,
    NameCid,
  },
  ipld_error::IpldError,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DefinitionSafety {
  Unsafe,
  Safe,
  Partial,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum QuotKind {
  Type,
  Ctor,
  Lift,
  Ind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecursorRule {
  ctor: NameCid,
  fields: BigUint,
  rhs: ExprCid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Const {
  Quotient {
    levels: BigUint,
    typ: ExprCid,
    kind: QuotKind,
  },
  Axiom {
    levels: BigUint,
    typ: ExprCid,
    is_unsafe: bool,
  },
  Definition {
    levels: BigUint,
    typ: ExprCid,
    val: ExprCid,
    safety: DefinitionSafety,
  },
  Theorem {
    levels: BigUint,
    typ: ExprCid,
    val: ExprCid,
  },
  Opaque {
    levels: BigUint,
    typ: ExprCid,
    val: ExprCid,
    is_unsafe: bool,
  },
  Inductive {
    levels: BigUint,
    typ: ExprCid,
    params: BigUint,
    intros: Vector<(NameCid, ExprCid)>,
    is_unsafe: bool,
  },
  Ctor {
    levels: BigUint,
    typ: ExprCid,
    induct: ConstCid,
    cidx: BigUint,
    params: BigUint,
    fields: BigUint,
    is_unsafe: bool,
  },
  Rec {
    levels: BigUint,
    typ: ExprCid,
    induct: ConstCid,
    params: BigUint,
    indices: BigUint,
    motives: BigUint,
    minors: BigUint,
    rules: Vec<RecursorRule>,
    k: bool,
    is_unsafe: bool,
  },
}

impl QuotKind {
  pub fn to_ipld(self) -> Ipld {
    match self {
      QuotKind::Type => Ipld::Integer(0),
      QuotKind::Ctor => Ipld::Integer(1),
      QuotKind::Lift => Ipld::Integer(2),
      QuotKind::Ind => Ipld::Integer(3),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(QuotKind::Type),
      Ipld::Integer(1) => Ok(QuotKind::Ctor),
      Ipld::Integer(2) => Ok(QuotKind::Lift),
      Ipld::Integer(3) => Ok(QuotKind::Ind),
      _ => Err(IpldError::QuotKind(ipld.clone())),
    }
  }
}

impl DefinitionSafety {
  pub fn to_ipld(self) -> Ipld {
    match self {
      DefinitionSafety::Unsafe => Ipld::Integer(0),
      DefinitionSafety::Safe => Ipld::Integer(1),
      DefinitionSafety::Partial => Ipld::Integer(2),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(DefinitionSafety::Unsafe),
      Ipld::Integer(1) => Ok(DefinitionSafety::Safe),
      Ipld::Integer(2) => Ok(DefinitionSafety::Partial),
      _ => Err(IpldError::DefinitionSafety(ipld.clone())),
    }
  }
}

impl RecursorRule {
  pub fn to_ipld(self) -> Ipld {
    Ipld::List(vec![
      Ipld::Link(self.ctor.0),
      Ipld::Bytes(self.fields.to_bytes_be()),
      Ipld::Link(self.rhs.0),
    ])
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [Link(ctor), Bytes(bs), Link(rhs)] => {
          let ctor = NameCid::from_cid(*ctor)?;
          let fields = BigUint::from_bytes_be(bs);
          let rhs = ExprCid::from_cid(*rhs)?;
          Ok(RecursorRule { ctor, fields, rhs })
        }
        xs => Err(IpldError::RecursorRule(List(xs.to_owned()))),
      },
      _ => Err(IpldError::RecursorRule(ipld.clone())),
    }
  }
}

impl Const {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Quotient { levels, typ, kind } => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Integer(0),
        Ipld::Bytes(levels.to_bytes_be()),
        kind.to_ipld(),
      ]),
      Self::Axiom { levels, typ, is_unsafe } => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Integer(1),
        Ipld::Bytes(levels.to_bytes_be()),
        Ipld::Link(typ.0),
        Ipld::Bool(*is_unsafe),
      ]),
      Self::Definition { levels, typ, val, safety } => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Integer(2),
        Ipld::Bytes(levels.to_bytes_be()),
        Ipld::Link(typ.0),
        Ipld::Link(val.0),
        safety.to_ipld(),
      ]),
      Self::Theorem { levels, typ, val } => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Integer(3),
        Ipld::Bytes(levels.to_bytes_be()),
        Ipld::Link(typ.0),
        Ipld::Link(val.0),
      ]),
      Self::Opaque { levels, typ, val, is_unsafe } => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Integer(4),
        Ipld::Bytes(levels.to_bytes_be()),
        Ipld::Link(typ.0),
        Ipld::Link(val.0),
        Ipld::Bool(*is_unsafe),
      ]),
      Self::Inductive { levels, typ, params, intros, is_unsafe } => {
        Ipld::List(vec![
          Ipld::Integer(5),
          Ipld::Integer(5),
          Ipld::Bytes(levels.to_bytes_be()),
          Ipld::Link(typ.0),
          Ipld::Bytes(params.to_bytes_be()),
          Ipld::List(
            intros
              .into_iter()
              .map(|(n, x)| Ipld::List(vec![Ipld::Link(n.0), Ipld::Link(x.0)]))
              .collect(),
          ),
          Ipld::Bool(*is_unsafe),
        ])
      }
      Self::Ctor { levels, typ, induct, cidx, params, fields, is_unsafe } => {
        Ipld::List(vec![
          Ipld::Integer(5),
          Ipld::Integer(6),
          Ipld::Bytes(levels.to_bytes_be()),
          Ipld::Link(typ.0),
          Ipld::Link(induct.0),
          Ipld::Bytes(cidx.to_bytes_be()),
          Ipld::Bytes(params.to_bytes_be()),
          Ipld::Bytes(fields.to_bytes_be()),
          Ipld::Bool(*is_unsafe),
        ])
      }
      Self::Rec {
        levels,
        typ,
        induct,
        params,
        indices,
        motives,
        minors,
        rules,
        k,
        is_unsafe,
      } => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Integer(7),
        Ipld::Bytes(levels.to_bytes_be()),
        Ipld::Link(typ.0),
        Ipld::Link(induct.0),
        Ipld::Bytes(params.to_bytes_be()),
        Ipld::Bytes(indices.to_bytes_be()),
        Ipld::Bytes(motives.to_bytes_be()),
        Ipld::Bytes(minors.to_bytes_be()),
        Ipld::List(rules.into_iter().map(|x| x.clone().to_ipld()).collect()),
        Ipld::Bool(*k),
        Ipld::Bool(*is_unsafe),
      ]),
    }
  }

  pub fn cid(&self) -> ConstCid {
    ConstCid::new(Code::Blake3_256.digest(
      DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
    ))
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(5), Integer(0), Bytes(ls), Link(t), kind] => {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let kind = QuotKind::from_ipld(kind)?;
          Ok(Const::Quotient { levels, typ, kind })
        }
        [Integer(5), Integer(1), Bytes(ls), Link(t), Bool(u)] => {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          Ok(Const::Axiom { levels, typ, is_unsafe: *u })
        }
        [Integer(5), Integer(2), Bytes(ls), Link(t), Link(v), safety] => {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          let safety = DefinitionSafety::from_ipld(safety)?;
          Ok(Const::Definition { levels, typ, val, safety })
        }
        [Integer(5), Integer(3), Bytes(ls), Link(t), Link(v)] => {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          Ok(Const::Theorem { levels, typ, val })
        }
        [Integer(5), Integer(4), Bytes(ls), Link(t), Link(v), Bool(u)] => {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let val = ExprCid::from_cid(*v)?;
          Ok(Const::Opaque { levels, typ, val, is_unsafe: *u })
        }
        [Integer(5), Integer(5), Bytes(ls), Link(t), Bytes(ps), List(is), Bool(u)] =>
        {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let params = BigUint::from_bytes_be(ps);
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
          Ok(Const::Inductive {
            levels,
            typ,
            params,
            intros: Vector::from(intros),
            is_unsafe: *u,
          })
        }
        [Integer(5), Integer(6), Bytes(ls), Link(t), Link(i), Bytes(c), Bytes(ps), Bytes(fs), Bool(u)] =>
        {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let induct = ConstCid::from_cid(*i)?;
          let cidx = BigUint::from_bytes_be(c);
          let params = BigUint::from_bytes_be(ps);
          let fields = BigUint::from_bytes_be(fs);
          Ok(Const::Ctor {
            levels,
            typ,
            induct,
            cidx,
            params,
            fields,
            is_unsafe: *u,
          })
        }
        [Integer(5), Integer(7), Bytes(ls), Link(t), Link(i), Bytes(ps), Bytes(is), Bytes(mos), Bytes(ms), List(rs), Bool(k), Bool(u)] =>
        {
          let levels = BigUint::from_bytes_be(ls);
          let typ = ExprCid::from_cid(*t)?;
          let induct = ConstCid::from_cid(*i)?;
          let params = BigUint::from_bytes_be(ps);
          let indices = BigUint::from_bytes_be(is);
          let motives = BigUint::from_bytes_be(mos);
          let minors = BigUint::from_bytes_be(ms);
          let mut rules = Vec::new();
          for r in rs {
            rules.push(RecursorRule::from_ipld(r)?)
          }
          Ok(Const::Rec {
            levels,
            typ,
            induct,
            params,
            indices,
            motives,
            minors,
            rules,
            k: *k,
            is_unsafe: *u,
          })
        }
        xs => Err(IpldError::Const(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Const(xs.to_owned())),
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

  impl Arbitrary for QuotKind {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> QuotKind>)> = vec![
        (1, Box::new(|_| QuotKind::Type)),
        (1, Box::new(|_| QuotKind::Ctor)),
        (1, Box::new(|_| QuotKind::Lift)),
        (1, Box::new(|_| QuotKind::Ind)),
      ];
      frequency(g, input)
    }
  }
  impl Arbitrary for DefinitionSafety {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> DefinitionSafety>)> = vec![
        (1, Box::new(|_| DefinitionSafety::Unsafe)),
        (1, Box::new(|_| DefinitionSafety::Safe)),
        (1, Box::new(|_| DefinitionSafety::Partial)),
      ];
      frequency(g, input)
    }
  }

  impl Arbitrary for RecursorRule {
    fn arbitrary(g: &mut Gen) -> Self {
      RecursorRule {
        ctor: Arbitrary::arbitrary(g),
        fields: arbitrary_big_uint()(g),
        rhs: Arbitrary::arbitrary(g),
      }
    }
  }

  impl Arbitrary for Const {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Const>)> = vec![
        (1, Box::new(|g| Const::Quotient { kind: Arbitrary::arbitrary(g) })),
        (
          1,
          Box::new(|g| Const::Axiom {
            levels: arbitrary_big_uint()(g),
            typ: Arbitrary::arbitrary(g),
            is_unsafe: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Const::Theorem {
            levels: arbitrary_big_uint()(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Const::Opaque {
            levels: arbitrary_big_uint()(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
            is_unsafe: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Const::Definition {
            levels: arbitrary_big_uint()(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
            safety: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| {
            let mut intros = Vec::new();
            let num = gen_range(g, 0..6);
            for _ in 0..num {
              intros.push((Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)));
            }
            Const::Inductive {
              levels: arbitrary_big_uint()(g),
              typ: Arbitrary::arbitrary(g),
              params: arbitrary_big_uint()(g),
              intros: Vector::from(intros),
              is_unsafe: Arbitrary::arbitrary(g),
            }
          }),
        ),
        (
          1,
          Box::new(|g| Const::Ctor {
            levels: arbitrary_big_uint()(g),
            typ: Arbitrary::arbitrary(g),
            induct: Arbitrary::arbitrary(g),
            cidx: arbitrary_big_uint()(g),
            params: arbitrary_big_uint()(g),
            fields: arbitrary_big_uint()(g),
            is_unsafe: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| {
            let mut rules = Vec::new();
            let num = gen_range(g, 0..6);
            for _ in 0..num {
              rules.push(Arbitrary::arbitrary(g));
            }
            Const::Rec {
              levels: arbitrary_big_uint()(g),
              typ: Arbitrary::arbitrary(g),
              induct: Arbitrary::arbitrary(g),
              params: arbitrary_big_uint()(g),
              indices: arbitrary_big_uint()(g),
              motives: arbitrary_big_uint()(g),
              minors: arbitrary_big_uint()(g),
              rules,
              k: Arbitrary::arbitrary(g),
              is_unsafe: Arbitrary::arbitrary(g),
            }
          }),
        ),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn const_ipld(x: Const) -> bool {
    match Const::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

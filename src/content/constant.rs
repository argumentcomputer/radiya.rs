use crate::content::{
  cid::{
    ConstCid,
    ExprCid,
    NameCid,
    CONSTANT,
  },
  ipld::{
    IpldEmbed,
    IpldError,
  },
};

use crate::constant::{
  DefinitionSafety,
  QuotKind,
};

use alloc::borrow::ToOwned;

use num_bigint::BigUint;
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
pub struct RecursorRule {
  ctor: NameCid,
  fields: BigUint,
  rhs: ExprCid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Intro {
  ctor: NameCid,
  typ: ExprCid,
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
    intros: Vec<Intro>,
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

impl IpldEmbed for QuotKind {
  fn to_ipld(&self) -> Ipld {
    match self {
      QuotKind::Type => Ipld::Integer(0),
      QuotKind::Ctor => Ipld::Integer(1),
      QuotKind::Lift => Ipld::Integer(2),
      QuotKind::Ind => Ipld::Integer(3),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(QuotKind::Type),
      Ipld::Integer(1) => Ok(QuotKind::Ctor),
      Ipld::Integer(2) => Ok(QuotKind::Lift),
      Ipld::Integer(3) => Ok(QuotKind::Ind),
      _ => Err(IpldError::expected("QuotKind", ipld)),
    }
  }
}

impl IpldEmbed for DefinitionSafety {
  fn to_ipld(&self) -> Ipld {
    match self {
      DefinitionSafety::Unsafe => Ipld::Integer(0),
      DefinitionSafety::Safe => Ipld::Integer(1),
      DefinitionSafety::Partial => Ipld::Integer(2),
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(DefinitionSafety::Unsafe),
      Ipld::Integer(1) => Ok(DefinitionSafety::Safe),
      Ipld::Integer(2) => Ok(DefinitionSafety::Partial),
      _ => Err(IpldError::expected("DefinitionSafety", ipld)),
    }
  }
}

impl IpldEmbed for Intro {
  fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![self.ctor.to_ipld(), self.typ.to_ipld()])
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [ctor, typ] => {
          let ctor = NameCid::from_ipld(ctor)?;
          let typ = ExprCid::from_ipld(typ)?;
          Ok(Intro { ctor, typ })
        }
        xs => Err(IpldError::expected("Intro", &Ipld::List(xs.to_owned()))),
      },
      _ => Err(IpldError::expected("Intro", ipld)),
    }
  }
}

impl IpldEmbed for RecursorRule {
  fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      self.ctor.to_ipld(),
      self.fields.to_ipld(),
      self.rhs.to_ipld(),
    ])
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [ctor, fields, rhs] => {
          let ctor = NameCid::from_ipld(ctor)?;
          let fields = BigUint::from_ipld(fields)?;
          let rhs = ExprCid::from_ipld(rhs)?;
          Ok(RecursorRule { ctor, fields, rhs })
        }
        xs => {
          Err(IpldError::expected("RecursorRule", &Ipld::List(xs.to_owned())))
        }
      },
      _ => Err(IpldError::expected("RecursorRule", ipld)),
    }
  }
}

impl Const {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Quotient { levels, typ, kind } => Ipld::List(vec![
        Ipld::Integer(CONSTANT.into()),
        Ipld::Integer(0),
        levels.to_ipld(),
        typ.to_ipld(),
        kind.to_ipld(),
      ]),
      Self::Axiom { levels, typ, is_unsafe } => Ipld::List(vec![
        Ipld::Integer(CONSTANT.into()),
        Ipld::Integer(1),
        levels.to_ipld(),
        typ.to_ipld(),
        is_unsafe.to_ipld(),
      ]),
      Self::Definition { levels, typ, val, safety } => Ipld::List(vec![
        Ipld::Integer(CONSTANT.into()),
        Ipld::Integer(2),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
        safety.to_ipld(),
      ]),
      Self::Theorem { levels, typ, val } => Ipld::List(vec![
        Ipld::Integer(CONSTANT.into()),
        Ipld::Integer(3),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
      ]),
      Self::Opaque { levels, typ, val, is_unsafe } => Ipld::List(vec![
        Ipld::Integer(CONSTANT.into()),
        Ipld::Integer(4),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
        is_unsafe.to_ipld(),
      ]),
      Self::Inductive { levels, typ, params, intros, is_unsafe } => {
        Ipld::List(vec![
          Ipld::Integer(CONSTANT.into()),
          Ipld::Integer(5),
          levels.to_ipld(),
          typ.to_ipld(),
          params.to_ipld(),
          intros.to_ipld(),
          is_unsafe.to_ipld(),
        ])
      }
      Self::Ctor { levels, typ, induct, cidx, params, fields, is_unsafe } => {
        Ipld::List(vec![
          Ipld::Integer(CONSTANT.into()),
          Ipld::Integer(6),
          levels.to_ipld(),
          typ.to_ipld(),
          induct.to_ipld(),
          cidx.to_ipld(),
          params.to_ipld(),
          fields.to_ipld(),
          is_unsafe.to_ipld(),
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
        Ipld::Integer(CONSTANT.into()),
        Ipld::Integer(7),
        levels.to_ipld(),
        typ.to_ipld(),
        induct.to_ipld(),
        params.to_ipld(),
        indices.to_ipld(),
        motives.to_ipld(),
        minors.to_ipld(),
        rules.to_ipld(),
        k.to_ipld(),
        is_unsafe.to_ipld(),
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
    let tag: i128 = CONSTANT.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0), levels, typ, kind] if *t == tag => {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let kind = QuotKind::from_ipld(kind)?;
          Ok(Const::Quotient { levels, typ, kind })
        }
        [Integer(t), Integer(1), levels, typ, is_unsafe] if *t == tag => {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Axiom { levels, typ, is_unsafe })
        }
        [Integer(t), Integer(2), levels, typ, val, safety] if *t == tag => {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let val = ExprCid::from_ipld(val)?;
          let safety = DefinitionSafety::from_ipld(safety)?;
          Ok(Const::Definition { levels, typ, val, safety })
        }
        [Integer(t), Integer(3), levels, typ, val] if *t == tag => {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let val = ExprCid::from_ipld(val)?;
          Ok(Const::Theorem { levels, typ, val })
        }
        [Integer(t), Integer(4), levels, typ, val, is_unsafe] if *t == tag => {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let val = ExprCid::from_ipld(val)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Opaque { levels, typ, val, is_unsafe })
        }
        [Integer(t), Integer(5), levels, typ, params, intros, is_unsafe]
          if *t == tag =>
        {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let params = BigUint::from_ipld(params)?;
          let intros = IpldEmbed::from_ipld(intros)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Inductive { levels, typ, params, intros, is_unsafe })
        }
        [Integer(t), Integer(6), levels, typ, induct, cidx, params, fields, is_unsafe]
          if *t == tag =>
        {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let induct = ConstCid::from_ipld(induct)?;
          let cidx = BigUint::from_ipld(cidx)?;
          let params = BigUint::from_ipld(params)?;
          let fields = BigUint::from_ipld(fields)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Ctor {
            levels,
            typ,
            induct,
            cidx,
            params,
            fields,
            is_unsafe,
          })
        }
        [Integer(t), Integer(7), levels, typ, induct, params, indices, motives, minors, rules, k, is_unsafe]
          if *t == tag =>
        {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let induct = ConstCid::from_ipld(induct)?;
          let params = BigUint::from_ipld(params)?;
          let indices = BigUint::from_ipld(indices)?;
          let motives = BigUint::from_ipld(motives)?;
          let minors = BigUint::from_ipld(minors)?;
          let rules = IpldEmbed::from_ipld(rules)?;
          let k = bool::from_ipld(k)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Rec {
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
          })
        }
        xs => Err(IpldError::expected("Const", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("Const", xs)),
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

  impl Arbitrary for Intro {
    fn arbitrary(g: &mut Gen) -> Self {
      Intro { ctor: Arbitrary::arbitrary(g), typ: Arbitrary::arbitrary(g) }
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
        (
          1,
          Box::new(|g| Const::Quotient {
            levels: arbitrary_big_uint()(g),
            typ: Arbitrary::arbitrary(g),
            kind: Arbitrary::arbitrary(g),
          }),
        ),
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
              intros.push(Arbitrary::arbitrary(g));
            }
            Const::Inductive {
              levels: arbitrary_big_uint()(g),
              typ: Arbitrary::arbitrary(g),
              params: arbitrary_big_uint()(g),
              intros,
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

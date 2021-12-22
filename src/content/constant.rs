use crate::{
  constant::{
    DefinitionSafety,
    QuotKind,
  },
  content::{
    cid::{
      ConstCid,
      ConstMetaCid,
      ExprCid,
      ExprMetaCid,
      NameCid,
      CONST,
      CONST_META,
    },
    ipld::{
      IpldEmbed,
      IpldError,
    },
  },
  parse::position::Pos,
};

use alloc::borrow::ToOwned;

use num_bigint::BigUint;
use sp_cid::Cid;
use sp_ipld::Ipld;

use sp_std::vec::Vec;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecursorRule {
  pub ctor: NameCid,
  pub fields: BigUint,
  pub rhs: ExprCid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Intro {
  pub ctor: NameCid,
  pub typ: ExprCid,
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
    uid: Cid,
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
    uid: Cid,
  },
  Inductive {
    levels: BigUint,
    typ: ExprCid,
    params: BigUint,
    indices: BigUint,
    intros: Vec<Intro>,
    is_unsafe: bool,
  },
  Constructor {
    levels: BigUint,
    typ: ExprCid,
    induct: ConstCid,
    cidx: BigUint,
    params: BigUint,
    fields: BigUint,
    is_unsafe: bool,
  },
  Recursor {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstMeta {
  Quotient {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
  },
  Axiom {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
  },
  Theorem {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
    val: ExprMetaCid,
  },
  Opaque {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
    val: ExprMetaCid,
  },
  Definition {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
    val: ExprMetaCid,
  },
  Inductive {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
    ctors: Vec<ExprMetaCid>,
  },
  Constructor {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
    induct: ConstMetaCid,
  },
  Recursor {
    pos: Pos,
    name: NameCid,
    levels: Vec<NameCid>,
    typ: ExprMetaCid,
    induct: ConstMetaCid,
    rules: Vec<ExprMetaCid>,
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

impl IpldEmbed for Const {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Quotient { levels, typ, kind } => Ipld::List(vec![
        Ipld::Integer(CONST.into()),
        Ipld::Integer(0),
        levels.to_ipld(),
        typ.to_ipld(),
        kind.to_ipld(),
      ]),
      Self::Axiom { levels, typ, is_unsafe, uid } => Ipld::List(vec![
        Ipld::Integer(CONST.into()),
        Ipld::Integer(1),
        levels.to_ipld(),
        typ.to_ipld(),
        is_unsafe.to_ipld(),
        Ipld::Link(*uid),
      ]),
      Self::Definition { levels, typ, val, safety } => Ipld::List(vec![
        Ipld::Integer(CONST.into()),
        Ipld::Integer(2),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
        safety.to_ipld(),
      ]),
      Self::Theorem { levels, typ, val } => Ipld::List(vec![
        Ipld::Integer(CONST.into()),
        Ipld::Integer(3),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
      ]),
      Self::Opaque { levels, typ, val, is_unsafe, uid } => Ipld::List(vec![
        Ipld::Integer(CONST.into()),
        Ipld::Integer(4),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
        is_unsafe.to_ipld(),
        Ipld::Link(*uid),
      ]),
      Self::Inductive { levels, typ, params, indices, intros, is_unsafe } => {
        Ipld::List(vec![
          Ipld::Integer(CONST.into()),
          Ipld::Integer(5),
          levels.to_ipld(),
          typ.to_ipld(),
          params.to_ipld(),
          indices.to_ipld(),
          intros.to_ipld(),
          is_unsafe.to_ipld(),
        ])
      }
      Self::Constructor {
        levels,
        typ,
        induct,
        cidx,
        params,
        fields,
        is_unsafe,
      } => Ipld::List(vec![
        Ipld::Integer(CONST.into()),
        Ipld::Integer(6),
        levels.to_ipld(),
        typ.to_ipld(),
        induct.to_ipld(),
        cidx.to_ipld(),
        params.to_ipld(),
        fields.to_ipld(),
        is_unsafe.to_ipld(),
      ]),
      Self::Recursor {
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
        Ipld::Integer(CONST.into()),
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

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = CONST.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0), levels, typ, kind] if *t == tag => {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let kind = QuotKind::from_ipld(kind)?;
          Ok(Const::Quotient { levels, typ, kind })
        }
        [Integer(t), Integer(1), levels, typ, is_unsafe, Link(uid)]
          if *t == tag =>
        {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Axiom { levels, typ, is_unsafe, uid: *uid })
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
        [Integer(t), Integer(4), levels, typ, val, is_unsafe, Link(uid)]
          if *t == tag =>
        {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let val = ExprCid::from_ipld(val)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Opaque { levels, typ, val, is_unsafe, uid: *uid })
        }
        [Integer(t), Integer(5), levels, typ, params, indices, intros, is_unsafe]
          if *t == tag =>
        {
          let levels = BigUint::from_ipld(levels)?;
          let typ = ExprCid::from_ipld(typ)?;
          let params = BigUint::from_ipld(params)?;
          let indices = BigUint::from_ipld(indices)?;
          let intros = IpldEmbed::from_ipld(intros)?;
          let is_unsafe = bool::from_ipld(is_unsafe)?;
          Ok(Const::Inductive {
            levels,
            typ,
            params,
            indices,
            intros,
            is_unsafe,
          })
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
          Ok(Const::Constructor {
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
          Ok(Const::Recursor {
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

impl IpldEmbed for ConstMeta {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Quotient { pos, name, levels, typ } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(0),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
      ]),
      Self::Axiom { pos, name, levels, typ } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(1),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
      ]),
      Self::Definition { pos, name, levels, typ, val } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(2),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
      ]),
      Self::Theorem { pos, name, levels, typ, val } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(3),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
      ]),
      Self::Opaque { pos, name, levels, typ, val } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(4),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
        val.to_ipld(),
      ]),
      Self::Inductive { pos, name, levels, typ, ctors } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(5),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
        ctors.to_ipld(),
      ]),
      Self::Constructor { pos, name, levels, typ, induct } => Ipld::List(vec![
        Ipld::Integer(CONST_META.into()),
        Ipld::Integer(6),
        pos.to_ipld(),
        name.to_ipld(),
        levels.to_ipld(),
        typ.to_ipld(),
        induct.to_ipld(),
      ]),
      Self::Recursor { pos, name, levels, typ, induct, rules } => {
        Ipld::List(vec![
          Ipld::Integer(CONST_META.into()),
          Ipld::Integer(7),
          pos.to_ipld(),
          name.to_ipld(),
          levels.to_ipld(),
          typ.to_ipld(),
          induct.to_ipld(),
          rules.to_ipld(),
        ])
      }
    }
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    let tag: i128 = CONST_META.into();
    match ipld {
      List(xs) => match xs.as_slice() {
        [Integer(t), Integer(0), pos, name, levels, typ] if *t == tag => {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          Ok(ConstMeta::Quotient { pos, name, levels, typ })
        }
        [Integer(t), Integer(1), pos, name, levels, typ] if *t == tag => {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          Ok(ConstMeta::Axiom { pos, name, levels, typ })
        }
        [Integer(t), Integer(2), pos, name, levels, typ, val] if *t == tag => {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          let val = ExprMetaCid::from_ipld(val)?;
          Ok(ConstMeta::Definition { pos, name, levels, typ, val })
        }
        [Integer(t), Integer(3), pos, name, levels, typ, val] if *t == tag => {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          let val = ExprMetaCid::from_ipld(val)?;
          Ok(ConstMeta::Theorem { pos, name, levels, typ, val })
        }
        [Integer(t), Integer(4), pos, name, levels, typ, val] if *t == tag => {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          let val = ExprMetaCid::from_ipld(val)?;
          Ok(ConstMeta::Opaque { pos, name, levels, typ, val })
        }
        [Integer(t), Integer(5), pos, name, levels, typ, ctors]
          if *t == tag =>
        {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          let ctors = IpldEmbed::from_ipld(ctors)?;
          Ok(ConstMeta::Inductive { pos, name, levels, typ, ctors })
        }
        [Integer(t), Integer(6), pos, name, levels, typ, induct]
          if *t == tag =>
        {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          let induct = ConstMetaCid::from_ipld(induct)?;
          Ok(ConstMeta::Constructor { pos, name, levels, typ, induct })
        }
        [Integer(t), Integer(7), pos, name, levels, typ, induct, rules]
          if *t == tag =>
        {
          let pos = Pos::from_ipld(pos)?;
          let name = NameCid::from_ipld(name)?;
          let levels = IpldEmbed::from_ipld(levels)?;
          let typ = ExprMetaCid::from_ipld(typ)?;
          let induct = ConstMetaCid::from_ipld(induct)?;
          let rules = IpldEmbed::from_ipld(rules)?;
          Ok(ConstMeta::Recursor { pos, name, levels, typ, induct, rules })
        }
        xs => Err(IpldError::expected("ConstMeta", &List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("ConstMeta", xs)),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::{
    content::cid::{
      tests::arbitrary_cid,
      NAME,
    },
    tests::{
      arbitrary_big_uint,
      frequency,
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
            uid: arbitrary_cid(g, NAME),
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
            uid: arbitrary_cid(g, NAME),
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
              indices: arbitrary_big_uint()(g),
              intros,
              is_unsafe: Arbitrary::arbitrary(g),
            }
          }),
        ),
        (
          1,
          Box::new(|g| Const::Constructor {
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
            Const::Recursor {
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

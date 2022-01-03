use crate::{
  expression::Expression,
  name::Name,
  parse::position::Pos,
};
use sp_im::Vector;
use sp_std::{
  fmt,
  fmt::Display,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum QuotKind {
  Type,
  Ctor,
  Lift,
  Ind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DefinitionSafety {
  Unsafe,
  Safe,
  Partial,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecursorRule {
  pub ctor: Name,
  pub num_fields: usize,
  pub rhs: Expression,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Intro {
  pub ctor: Name,
  pub typ: Expression,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Constant {
  Quotient {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    kind: QuotKind,
  },
  Axiom {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    is_unsafe: bool,
  },
  Theorem {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    val: Expression,
  },
  Opaque {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    val: Expression,
    is_unsafe: bool,
  },
  Definition {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    val: Expression,
    safety: DefinitionSafety,
  },
  Inductive {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    intros: Vector<Intro>,
    params: usize,
    indices: usize,
    is_unsafe: bool,
  },
  Constructor {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    induct: Name,
    ctor_idx: usize,
    params: usize,
    fields: usize,
    is_unsafe: bool,
  },
  Recursor {
    pos: Pos,
    name: Name,
    level_params: Vector<Name>,
    typ: Expression,
    induct: Name,
    params: usize,
    indices: usize,
    motives: usize,
    minors: usize,
    rules: Vector<RecursorRule>,
    k: bool,
    is_unsafe: bool,
  },
}

impl Display for QuotKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Type => {
        write!(f, "Type")
      }
      Self::Ctor => {
        write!(f, "Ctor")
      }
      Self::Lift => {
        write!(f, "Lift")
      }
      Self::Ind => {
        write!(f, "Ind")
      }
    }
  }
}

impl Display for DefinitionSafety {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Unsafe => {
        write!(f, "Unsafe")
      }
      Self::Safe => {
        write!(f, "Safe")
      }
      Self::Partial => {
        write!(f, "Partial")
      }
    }
  }
}

#[cfg(test)]
pub mod tests {
  use crate::tests::{
    arbitrary_big_uint,
    frequency,
    gen_range,
    next_case,
  };
  use sp_std::mem;

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
        num_fields: Arbitrary::arbitrary(g),
        rhs: Arbitrary::arbitrary(g),
      }
    }
  }
  impl Arbitrary for Intro {
    fn arbitrary(g: &mut Gen) -> Self {
      Intro { ctor: Arbitrary::arbitrary(g), typ: Arbitrary::arbitrary(g) }
    }
  }

  fn arbitrary_vector<T: Clone + Arbitrary>(g: &mut Gen) -> Vector<T> {
    let mut vec = Vec::new();
    let x: usize = Arbitrary::arbitrary(g);
    let x = x % 6;
    for _ in 0..x {
      vec.push(Arbitrary::arbitrary(g));
    }
    vec.into()
  }

  impl Arbitrary for Constant {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Constant>)> = vec![
        (
          1,
          Box::new(|g| Constant::Quotient {
            pos: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            level_params: arbitrary_vector(g),
            typ: Arbitrary::arbitrary(g),
            kind: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Constant::Axiom {
            pos: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            level_params: arbitrary_vector(g),
            typ: Arbitrary::arbitrary(g),
            is_unsafe: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Constant::Theorem {
            pos: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            level_params: arbitrary_vector(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Constant::Opaque {
            pos: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            level_params: arbitrary_vector(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
            is_unsafe: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Constant::Definition {
            pos: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            level_params: arbitrary_vector(g),
            typ: Arbitrary::arbitrary(g),
            val: Arbitrary::arbitrary(g),
            safety: Arbitrary::arbitrary(g),
          }),
        ),
        (
          1,
          Box::new(|g| Constant::Inductive {
            pos: Arbitrary::arbitrary(g),
            name: Arbitrary::arbitrary(g),
            level_params: arbitrary_vector(g),
            typ: Arbitrary::arbitrary(g),
            intros: arbitrary_vector(g),
            params: Arbitrary::arbitrary(g),
            indices: Arbitrary::arbitrary(g),
            is_unsafe: Arbitrary::arbitrary(g),
          }),
        ),
        //(
        //  1,
        //  Box::new(|g| Constant::Constructor {
        //    pos: Arbitrary::arbitrary(g),
        //    name: Arbitrary::arbitrary(g),
        //    level_params: arbitrary_vector(g),
        //    typ: Arbitrary::arbitrary(g),
        //    induct: Arbitrary::arbitrary(g),
        //    ctor_idx: Arbitrary::arbitrary(g),
        //    params: Arbitrary::arbitrary(g),
        //    fields: Arbitrary::arbitrary(g),
        //    is_unsafe: Arbitrary::arbitrary(g),
        //  }),
        //),
        //(
        //  1,
        //  Box::new(|g| Constant::Recursor {
        //    pos: Arbitrary::arbitrary(g),
        //    name: Arbitrary::arbitrary(g),
        //    level_params: arbitrary_vector(g),
        //    typ: Arbitrary::arbitrary(g),
        //    induct: Arbitrary::arbitrary(g),
        //    params: Arbitrary::arbitrary(g),
        //    indices: Arbitrary::arbitrary(g),
        //    motives: Arbitrary::arbitrary(g),
        //    minors: Arbitrary::arbitrary(g),
        //    rules: arbitrary_vector(g),
        //    k: Arbitrary::arbitrary(g),
        //    is_unsafe: Arbitrary::arbitrary(g),
        //  }),
        //),
      ];
      frequency(g, input)
    }
  }
}

use crate::name::Name;
use sp_std::{
  boxed::Box,
  mem::MaybeUninit,
  ptr::NonNull,
  vec::Vec,
};

/// Universe levels
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Universe {
  Zero,
  Succ(Box<Universe>),
  Max(Box<Universe>, Box<Universe>),
  IMax(Box<Universe>, Box<Universe>),
  Param(Name, usize),
}

pub fn simplify(lvl: &Universe) -> Box<Universe> {
  match lvl {
    Universe::Zero | Universe::Param(..) => Box::new(lvl.clone()),
    Universe::Succ(lvl) => Box::new(Universe::Succ(simplify(lvl))),
    Universe::Max(a, b) => Box::new(Universe::Max(simplify(a), simplify(b))),
    Universe::IMax(a, b) => {
      let b_prime = simplify(b);
      match &*b_prime {
        Universe::Zero => Box::new(Universe::Zero),
        Universe::Succ(..) => combining(&simplify(a), &b_prime),
        _ => Box::new(Universe::IMax(simplify(a), b_prime)),
      }
    }
  }
}

pub fn combining(lvl: &Box<Universe>, other: &Box<Universe>) -> Box<Universe> {
  match (&**lvl, &**other) {
    (Universe::Zero, _) => other.clone(),
    (_, Universe::Zero) => lvl.clone(),
    (Universe::Succ(lhs), Universe::Succ(rhs)) => {
      Box::new(Universe::Succ(combining(lhs, rhs)))
    }
    _ => Box::new(Universe::Max(lvl.clone(), other.clone())),
  }
}

pub fn is_same_level(lvl_a: &Universe, lvl_b: &Universe) -> bool {
  let lvl_a = simplify(lvl_a);
  let lvl_b = simplify(lvl_b);
  leq_core(&lvl_a, &lvl_b, 0) && leq_core(&lvl_b, &lvl_a, 0)
}

pub fn leq_core(
  lvl_a: &Box<Universe>,
  lvl_b: &Box<Universe>,
  diff: i32,
) -> bool {
  match (&**lvl_a, &**lvl_b) {
    (Universe::Zero, _) if diff >= 0 => true,
    (_, Universe::Zero) if diff < 0 => false,
    (Universe::Param(_, a), Universe::Param(_, x)) => a == x && diff >= 0,
    (Universe::Param(..), Universe::Zero) => false,
    (Universe::Zero, Universe::Param(..)) => diff >= 0,

    (Universe::Succ(s), _) => leq_core(s, lvl_b, diff - 1),
    (_, Universe::Succ(s)) => leq_core(lvl_a, s, diff + 1),

    (Universe::Max(a, b), _) => {
      leq_core(a, lvl_b, diff) && leq_core(b, lvl_b, diff)
    }

    (Universe::Param(..), Universe::Max(x, y)) => {
      leq_core(lvl_a, x, diff) || leq_core(lvl_a, y, diff)
    }

    (Universe::Zero, Universe::Max(x, y)) => {
      leq_core(lvl_a, x, diff) || leq_core(lvl_a, y, diff)
    }

    (Universe::IMax(a, b), Universe::IMax(x, y)) if a == x && b == y => true,

    (Universe::IMax(.., b), _) if is_param(b) => {
      ensure_imax_leq(b, lvl_a, lvl_b, diff)
    }

    (_, Universe::IMax(.., y)) if is_param(y) => {
      ensure_imax_leq(y, lvl_a, lvl_b, diff)
    }

    (Universe::IMax(a, b), _) if is_any_max(b) => match &**b {
      Universe::IMax(x, y) => {
        let new_max = Box::new(Universe::Max(
          Box::new(Universe::IMax(a.clone(), y.clone())),
          Box::new(Universe::IMax(x.clone(), y.clone())),
        ));
        leq_core(&new_max, lvl_b, diff)
      }

      Universe::Max(x, y) => {
        let new_max = simplify(&Box::new(Universe::Max(
          Box::new(Universe::IMax(a.clone(), x.clone())),
          Box::new(Universe::IMax(a.clone(), y.clone())),
        )));
        leq_core(&new_max, lvl_b, diff)
      }
      _ => unreachable!(),
    },

    (_, Universe::IMax(x, y)) if is_any_max(y) => match &**y {
      Universe::IMax(j, k) => {
        let new_max = Box::new(Universe::Max(
          Box::new(Universe::IMax(x.clone(), k.clone())),
          Box::new(Universe::IMax(j.clone(), k.clone())),
        ));
        leq_core(lvl_a, &new_max, diff)
      }
      Universe::Max(j, k) => {
        let new_max = simplify(&Box::new(Universe::Max(
          Box::new(Universe::IMax(x.clone(), j.clone())),
          Box::new(Universe::IMax(x.clone(), k.clone())),
        )));
        leq_core(lvl_a, &new_max, diff)
      }
      _ => unreachable!(),
    },
    _ => unreachable!(),
  }
}

pub fn ensure_imax_leq(
  imax: &Box<Universe>,
  lhs: &Box<Universe>,
  rhs: &Box<Universe>,
  diff: i32,
) -> bool {
  let zero_map = vec![(imax.clone(), Box::new(Universe::Zero))];
  let nonzero_map =
    vec![(imax.clone(), Box::new(Universe::Succ(imax.clone())))];

  let closure = |subst: &Vec<(Box<Universe>, Box<Universe>)>,
                 left: &Box<Universe>,
                 right: &Box<Universe>| {
    let left_prime = simplify(&instantiate_lvl(left, subst));
    let right_prime = simplify(&instantiate_lvl(right, subst));
    leq_core(&left_prime, &right_prime, diff)
  };

  closure(&zero_map, lhs, rhs) && closure(&nonzero_map, lhs, rhs)
}

pub fn instantiate_lvl(
  lvl: &Box<Universe>,
  substs: &Vec<(Box<Universe>, Box<Universe>)>,
) -> Box<Universe> {
  match &**lvl {
    Universe::Zero => Box::new(Universe::Zero),
    Universe::Succ(inner) => {
      Box::new(Universe::Succ(instantiate_lvl(inner, substs)))
    }
    Universe::Max(a, b) => {
      let a_prime = instantiate_lvl(a, substs);
      let b_prime = instantiate_lvl(b, substs);
      Box::new(Universe::IMax(a_prime, b_prime))
    }
    Universe::IMax(a, b) => {
      let a_prime = instantiate_lvl(a, substs);
      let b_prime = instantiate_lvl(b, substs);
      Box::new(Universe::IMax(a_prime, b_prime))
    }
    Universe::Param(..) => substs
      .iter()
      .find(|(l, _)| l == lvl)
      .map(|(_, r)| r.clone())
      .unwrap_or_else(|| lvl.clone()),
  }
}

pub fn is_param(lvl: &Box<Universe>) -> bool {
  match &**lvl {
    Universe::Param(..) => true,
    _ => false,
  }
}

pub fn is_any_max(lvl: &Box<Universe>) -> bool {
  match &**lvl {
    Universe::Max(..) | Universe::IMax(..) => true,
    _ => false,
  }
}

#[derive(Debug)]
pub enum GenUniverse {
  Zero,
  Succ(NonNull<MaybeUninit<GenUniverse>>),
  Max(NonNull<MaybeUninit<GenUniverse>>, NonNull<MaybeUninit<GenUniverse>>),
  IMax(NonNull<MaybeUninit<GenUniverse>>, NonNull<MaybeUninit<GenUniverse>>),
  Param(Name, usize),
}
#[cfg(test)]
pub mod tests {

  use crate::tests::{
    gen_range,
    next_case,
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use sp_im::vector::Vector;
  use sp_std::mem;

  #[derive(Debug, Clone, Copy)]
  pub enum UniverseCase {
    ZERO,
    SUCC,
    MAX,
    IMAX,
    PARAM,
  }

  #[inline]
  pub fn alloc_val<T>(val: T) -> NonNull<T> {
    NonNull::new(Box::leak(Box::new(val))).unwrap()
  }

  pub fn arbitrary_universe(g: &mut Gen, ctx: Vector<Name>) -> Universe {
    let res = alloc_val(MaybeUninit::<GenUniverse>::uninit());
    let mut stack = vec![res.clone()];
    let _term = Box::new(res);
    while let Some(mut ptr) = stack.pop() {
      let gens: Vec<(usize, UniverseCase)> = vec![
        (100, UniverseCase::ZERO),
        (if ctx.len() == 0 { 0 } else { 100 }, UniverseCase::PARAM),
        (50, UniverseCase::SUCC),
        (25, UniverseCase::MAX),
        (25, UniverseCase::IMAX),
      ];
      use GenUniverse::*;
      match next_case(g, &gens) {
        UniverseCase::ZERO => unsafe {
          *ptr.as_mut() = MaybeUninit::new(Zero);
        },
        UniverseCase::PARAM => unsafe {
          let idx = gen_range(g, 0..ctx.len());
          let nam = ctx[idx].clone();
          *ptr.as_mut() = MaybeUninit::new(Param(nam, idx));
        },
        UniverseCase::SUCC => {
          let pred = alloc_val(MaybeUninit::<GenUniverse>::uninit());
          stack.push(pred);
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(Succ(pred));
          }
        }
        UniverseCase::MAX => {
          let lhs = alloc_val(MaybeUninit::<GenUniverse>::uninit());
          let rhs = alloc_val(MaybeUninit::<GenUniverse>::uninit());
          stack.push(lhs);
          stack.push(rhs);
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(Max(lhs, rhs));
          }
        }
        UniverseCase::IMAX => {
          let lhs = alloc_val(MaybeUninit::<GenUniverse>::uninit());
          let rhs = alloc_val(MaybeUninit::<GenUniverse>::uninit());
          stack.push(lhs);
          stack.push(rhs);
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(IMax(lhs, rhs));
          }
        }
      }
    }
    unsafe {
      let term = Box::from_raw(res.as_ptr());
      let term = term.assume_init();
      mem::transmute::<GenUniverse, Universe>(Box::into_inner(term))
    }
  }
  impl Arbitrary for Universe {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut ctx = Vec::new();
      let num = gen_range(g, 0..6);
      for _ in 0..num {
        ctx.push(Arbitrary::arbitrary(g));
      }
      arbitrary_universe(g, Vector::from(ctx))
    }
  }
}

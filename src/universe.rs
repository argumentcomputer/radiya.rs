use crate::name::Name;
use sp_std::rc::Rc;
use sp_std::vec::Vec;

/// Universe levels
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Univ {
  Zero,
  Succ(Rc<Univ>),
  Max(Rc<Univ>, Rc<Univ>),
  IMax(Rc<Univ>, Rc<Univ>),
  Param(Name),
}

pub fn simplify(lvl: &Univ) -> Rc<Univ> {
  match lvl {
    Univ::Zero | Univ::Param(..) => Rc::new(lvl.clone()),
    Univ::Succ(lvl) => Rc::new(Univ::Succ(simplify(lvl))),
    Univ::Max(a, b) => Rc::new(Univ::Max(simplify(a), simplify(b))),
    Univ::IMax(a, b) => {
      let b_prime = simplify(b);
      match &*b_prime {
        Univ::Zero => Rc::new(Univ::Zero),
        Univ::Succ(..) => combining(&simplify(a), &b_prime),
        _ => Rc::new(Univ::IMax(simplify(a), b_prime))
      }
    }
  }
}

pub fn combining(lvl: &Rc<Univ>, other: &Rc<Univ>) -> Rc<Univ> {
  match (&**lvl, &**other) {
    (Univ::Zero, _) => other.clone(),
    (_, Univ::Zero) => lvl.clone(),
    (Univ::Succ(lhs), Univ::Succ(rhs)) => Rc::new(Univ::Succ(combining(lhs, rhs))),
    _ => Rc::new(Univ::Max(lvl.clone(), other.clone()))
  }
}

pub fn is_same_level(
  lvl_a: &Univ,
  lvl_b: &Univ,
) -> bool {
  let lvl_a = simplify(lvl_a);
  let lvl_b = simplify(lvl_b);
  leq_core(&lvl_a, &lvl_b, 0) && leq_core(&lvl_b, &lvl_a, 0)
}

pub fn leq_core(lvl_a: &Rc<Univ>, lvl_b: &Rc<Univ>, diff: i32) -> bool {
  match (&**lvl_a, &**lvl_b) {
    (Univ::Zero, _) if diff >= 0 => true,
    (_, Univ::Zero) if diff < 0 => false,
    (Univ::Param(a), Univ::Param(x)) => a == x && diff >= 0,
    (Univ::Param(..), Univ::Zero) => false,
    (Univ::Zero, Univ::Param(..)) => diff >= 0,

    (Univ::Succ(s), _) => leq_core(s, lvl_b, diff - 1),
    (_, Univ::Succ(s)) => leq_core(lvl_a, s, diff + 1),

    (Univ::Max(a, b), _) => leq_core(a, lvl_b, diff) && leq_core(b, lvl_b, diff),

    (Univ::Param(..), Univ::Max(x, y)) => leq_core(lvl_a, x, diff) || leq_core(lvl_a, y, diff),

    (Univ::Zero, Univ::Max(x, y)) => leq_core(lvl_a, x, diff) || leq_core(lvl_a, y, diff),

    (Univ::IMax(a, b), Univ::IMax(x, y)) if a == x && b == y => true,

    (Univ::IMax(.., b), _) if is_param(b) => ensure_imax_leq(b, lvl_a, lvl_b, diff),

    (_, Univ::IMax(.., y)) if is_param(y) => ensure_imax_leq(y, lvl_a, lvl_b, diff),

    (Univ::IMax(a, b), _) if is_any_max(b) => match &**b {
      Univ::IMax(x, y) => {
        let new_max = Rc::new(Univ::Max(Rc::new(Univ::IMax(a.clone(), y.clone())),
                                        Rc::new(Univ::IMax(x.clone(), y.clone()))));
        leq_core(&new_max, lvl_b, diff)
      },

      Univ::Max(x, y) => {
        let new_max = simplify
          (&Rc::new(Univ::Max(Rc::new(Univ::IMax(a.clone(), x.clone())),
                              Rc::new(Univ::IMax(a.clone(), y.clone())))));
        leq_core(&new_max, lvl_b, diff)

      },
      _ => unreachable!(),
    }

    (_, Univ::IMax(x, y)) if is_any_max(y) => match &**y {
      Univ::IMax(j, k) => {
        let new_max = Rc::new(Univ::Max(Rc::new(Univ::IMax(x.clone(), k.clone())),
                                        Rc::new(Univ::IMax(j.clone(), k.clone()))));
        leq_core(lvl_a, &new_max, diff)
      },
      Univ::Max(j, k) => {
        let new_max = simplify
          (&Rc::new(Univ::Max(Rc::new(Univ::IMax(x.clone(), j.clone())),
                              Rc::new(Univ::IMax(x.clone(), k.clone())))));
        leq_core(lvl_a, &new_max, diff)
      },
      _ => unreachable!(),
    }
    _ => unreachable!()
  }
}

pub fn ensure_imax_leq(imax: &Rc<Univ>, lhs: &Rc<Univ>, rhs: &Rc<Univ>, diff: i32) -> bool {

  let zero_map =  vec![(imax.clone(), Rc::new(Univ::Zero))];
  let nonzero_map = vec![(imax.clone(), Rc::new(Univ::Succ(imax.clone())))];


  let closure = |subst : &Vec<(Rc<Univ>, Rc<Univ>)>, left : &Rc<Univ>, right : &Rc<Univ>| {
    let left_prime  = simplify(&instantiate_lvl(left, subst));
    let right_prime = simplify(&instantiate_lvl(right, subst));
    leq_core(&left_prime, &right_prime, diff)
  };

  closure(&zero_map, lhs, rhs)
    &&
    closure(&nonzero_map, lhs, rhs)
}

pub fn instantiate_lvl(lvl: &Rc<Univ>, substs : &Vec<(Rc<Univ>, Rc<Univ>)>) -> Rc<Univ> {
  match &**lvl {
    Univ::Zero => Rc::new(Univ::Zero),
    Univ::Succ(inner) => Rc::new(Univ::Succ(instantiate_lvl(inner, substs))),
    Univ::Max(a, b) => {
      let a_prime = instantiate_lvl(a, substs);
      let b_prime = instantiate_lvl(b, substs);
      Rc::new(Univ::IMax(a_prime, b_prime)
)    },
    Univ::IMax(a, b) => {
      let a_prime = instantiate_lvl(a, substs);
      let b_prime = instantiate_lvl(b, substs);
      Rc::new(Univ::IMax(a_prime, b_prime))
    },
    Univ::Param(..) => {
      substs.iter()
        .find(|(l, _)| l == lvl)
        .map(|(_, r)| r.clone())
        .unwrap_or_else(|| lvl.clone())
    }
  }
}

pub fn is_param(lvl: &Rc<Univ>) -> bool {
  match &**lvl {
    Univ::Param(..) => true,
    _         => false
  }
}

pub fn is_any_max(lvl: &Rc<Univ>) -> bool {
  match &**lvl {
    Univ::Max(..) | Univ::IMax(..) => true,
    _                  => false
  }
}

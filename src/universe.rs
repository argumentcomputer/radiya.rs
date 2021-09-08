use crate::name::Name;
use sp_std::rc::Rc;

/// Universe levels
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Univ {
  Zero,
  Succ(Rc<Univ>),
  Max(Rc<Univ>, Rc<Univ>),
  IMax(Rc<Univ>, Rc<Univ>),
  Param(Name),
}

pub fn simplify(lvl: &Rc<Univ>) -> Rc<Univ> {
  match &**lvl {
    Univ::Zero | Univ::Param(..) => lvl.clone(),
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
  lvl_a: &Rc<Univ>,
  lvl_b: &Rc<Univ>,
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

    // (Univ::IMax(.., b), _) if b.is_param() => b.ensure_imax_leq(self, other, diff),

    // (_, Univ::IMax(.., y)) if y.is_param() => y.ensure_imax_leq(self, other, diff),

    // (Univ::IMax(a, b), _) if b.is_any_max() => match b.as_ref() {
    //   Univ::IMax(x, y) => {
    //     let new_max = mk_max(mk_imax_refs(a, y),
    //                          mk_imax_refs(x, y));
    //     Level::leq_core(&new_max, other, diff)
    //   },

    //   Univ::Max(x, y) => {
    //     let new_max = mk_max(mk_imax_refs(a, x),
    //                          mk_imax_refs(a, y)).simplify();
    //     Level::leq_core(&new_max, other, diff)

    //   },
    //   _ => unreachable!(),
    // }

    // (_, Univ::IMax(x, y)) if y.is_any_max() => match y.as_ref() {
    //   Univ::IMax(j, k) => {
    //     let new_max = mk_max(mk_imax_refs(x, k),
    //                          mk_imax_refs(j, k));
    //     self.leq_core(&new_max, diff)
    //   },
    //   Univ::Max(j, k) => {
    //     let new_max = mk_max(mk_imax_refs(x, j),
    //                          mk_imax_refs(x, k)).simplify();
    //     self.leq_core(&new_max, diff)
    //   },
    //   _ => unreachable!(),
    // }
    _ => unreachable!()
  }
}

// pub fn ensure_imax_leq(imax: &Rc<Univ> lhs: &Rc<Univ>, rhs: &Rc<Univ>, diff: i32) -> bool {

//   let zero_map =  vec![(imax.clone(), Rc::new(Univ::Zero))];
//   let nonzero_map = vec![(imax.clone(), Rc::new(Univ::Succ(imax.clone())))];


//   let closure = |subst : &Vec<(Rc<Univ>, Rc<Univ>)>, left : &Rc<Univ>, right : &Rc<Univ>| {
//     let left_prime  = left.instantiate_lvl(subst).simplify();
//     let right_prime = right.instantiate_lvl(subst).simplify();
//     left_prime.leq_core(&right_prime, diff)
//   };

//   closure(&zero_map, lhs, rhs)
//     &&
//     closure(&nonzero_map, lhs, rhs)
// }

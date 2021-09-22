use crate::{
  universe::is_same_level,
  expression::Expr,
};

use sp_std::vec::Vec;
use sp_std::rc::Rc;

// Assumes `val` has no unbound `BVar` nodes; i.e., that they have been
// replaced by `FVar` nodes
pub fn subst(bod: Rc<Expr>, idx: usize, val: Rc<Expr>) -> Rc<Expr> {
  match &*bod {
    Expr::BVar(jdx) => {
      if idx == *jdx {
        val
      }
      else if *jdx > idx {
        Rc::new(Expr::BVar(*jdx-1))
      }
      else {
        bod
      }
    }
    Expr::Lam(nam, bnd, dom, bod) => {
      let bod = subst(bod.clone(), idx+1, val.clone());
      let dom = subst(dom.clone(), idx, val);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = subst(fun.clone(), idx, val.clone());
      let arg = subst(arg.clone(), idx, val);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = subst(dom.clone(), idx, val.clone());
      let img = subst(img.clone(), idx+1, val);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = subst(typ.clone(), idx, val.clone());
      let exp = subst(exp.clone(), idx, val.clone());
      let bod = subst(bod.clone(), idx+1, val);
      Rc::new(Expr::Let(nam.clone(), typ, exp, bod))
    }
    _ => bod,
  }
}

pub fn whnf_unfold(
  top_node: Rc<Expr>
) -> (Rc<Expr>, Vec<Rc<Expr>>) {
  let mut node = top_node;
  let mut args = Vec::new();
  loop {
    let next_node = {
      match &*node {
        Expr::App(fun, arg) => {
          args.push(arg.clone());
          fun.clone()
        }
        Expr::Lam(_, _, _, bod) => {
          let len = args.len();
          if len > 0 {
            let arg = args.pop().unwrap();
            subst(bod.clone(), 0, arg)
          }
          else {
            break
          }
        },
        _ => break,
      }
    };
    node = next_node;
  }
  (node, args)
}

#[inline]
pub fn fold_args(
  mut head: Rc<Expr>,
  mut args: Vec<Rc<Expr>>,
) -> Rc<Expr> {
  while let Some(arg) = args.pop() {
    head = Rc::new(Expr::App(head, arg.clone()));
  }
  head
}

#[inline]
pub fn whnf(
  node: Rc<Expr>,
) -> Rc<Expr> {
  let (head, args) = whnf_unfold(node);
  fold_args(head, args)
}

pub fn equal(
  expr_a: &Rc<Expr>,
  expr_b: &Rc<Expr>,
  unique: &mut usize,
) -> bool {
  // Short circuit if they're the same pointer
  if Rc::as_ptr(expr_a) == Rc::as_ptr(expr_b) {
    return true
  }
  // Return true if they are proof irrelevant terms
  if is_proof_irrel(expr_a, expr_b) {
    return true
  }
  let (head_a, args_a) = whnf_unfold(expr_a.clone());
  let (head_b, args_b) = whnf_unfold(expr_b.clone());
  match (&*head_a, &*head_b) {
    (Expr::Lam(_, _, a_dom, a_bod), Expr::Lam(_, _, b_dom, b_bod)) => {
      if equal(a_dom, b_dom, unique) {
        let f_var = Rc::new(
          Expr::FVar(*unique)
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *unique = *unique + 1;
        return equal(&a_bod, &b_bod, unique) 
      }
      false
    }
    (Expr::Pi(_, _, a_dom, a_bod), Expr::Pi(_, _, b_dom, b_bod)) => {
      if equal(a_dom, b_dom, unique) {
        let f_var = Rc::new(
          Expr::FVar(*unique)
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *unique = *unique + 1;
        return equal(&a_bod, &b_bod, unique) 
      }
      false
    }
    (Expr::FVar(uniq_a), Expr::FVar(uniq_b)) => {
      *uniq_a == *uniq_b
    }
    (Expr::Sort(lvl_a), Expr::Sort(lvl_b)) => {
      is_same_level(lvl_a, lvl_b)
    }
    (Expr::Const(nam_a, lvls_a), Expr::Const(nam_b, lvls_b)) => {
      if nam_a == nam_b && lvls_a.iter().zip(lvls_b).all(|(a, b)| is_same_level(a, b)) {
        return args_a.iter().zip(args_b).all(|(a, b)| equal(&a, &b, unique))
      }
      false
    }
    (Expr::Lam(_, _, _, a_bod), _) => {
      let f_var = Rc::new(
        Expr::FVar(*unique)
      );
      *unique = *unique + 1;
      let a_bod = subst(a_bod.clone(), 0, f_var.clone());
      let b_bod = Rc::new(Expr::App(fold_args(head_b, args_b), f_var));
      equal(&a_bod, &b_bod, unique)
    }
    (_, Expr::Lam(_, _, _, b_bod)) => {
      let f_var = Rc::new(
        Expr::FVar(*unique)
      );
      *unique = *unique + 1;
      let b_bod = subst(b_bod.clone(), 0, f_var.clone());
      let a_bod = Rc::new(Expr::App(fold_args(head_a, args_a), f_var));
      equal(&a_bod, &b_bod, unique)
    }
    _ => false
  }
}

pub fn is_proof_irrel(
  _expr_a: &Rc<Expr>,
  _expr_b: &Rc<Expr>,
) -> bool {
  // TODO
  false
}

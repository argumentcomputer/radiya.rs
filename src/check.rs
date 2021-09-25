use crate::{
  name::Name,
  universe::{
    Univ,
    simplify,
    is_same_level,
  },
  expression::Expr,
};

use sp_std::vec::Vec;
use sp_im::vector::Vector;
use sp_std::rc::Rc;

// Assumes `val` has no unbound `BVar` nodes; i.e., that they have been
// replaced by `FVar` nodes
pub fn subst(bod: Rc<Expr>, ini: usize, val: Rc<Expr>) -> Rc<Expr> {
  match &*bod {
    Expr::BVar(idx) => {
      if *idx < ini {
        bod
      }
      // This should always be the case if we are not reducing inner lambdas
      else if ini == *idx {
        val
      }
      else {
        Rc::new(Expr::BVar(*idx-1))
      }
    }
    Expr::Lam(nam, bnd, dom, bod) => {
      let bod = subst(bod.clone(), ini+1, val.clone());
      let dom = subst(dom.clone(), ini, val);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = subst(fun.clone(), ini, val.clone());
      let arg = subst(arg.clone(), ini, val);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = subst(dom.clone(), ini, val.clone());
      let img = subst(img.clone(), ini+1, val);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = subst(typ.clone(), ini, val.clone());
      let exp = subst(exp.clone(), ini, val.clone());
      let bod = subst(bod.clone(), ini+1, val);
      Rc::new(Expr::Let(nam.clone(), typ, exp, bod))
    }
    _ => bod,
  }
}

// Analogously, assumes `vals` has no unbound `BVar` nodes
pub fn subst_bulk(bod: Rc<Expr>, ini: usize, vals: &[Rc<Expr>]) -> Rc<Expr> {
  match &*bod {
    Expr::BVar(idx) => {
      if *idx < ini {
        bod
      }
      // This should always be the case if we are not reducing inner lambdas
      else if *idx < ini+vals.len() {
        vals[idx-ini].clone()
      }
      else {
        Rc::new(Expr::BVar(*idx-vals.len()))
      }
    }
    Expr::Lam(nam, bnd, dom, bod) => {
      let bod = subst_bulk(bod.clone(), ini+1, vals);
      let dom = subst_bulk(dom.clone(), ini, vals);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = subst_bulk(fun.clone(), ini, vals);
      let arg = subst_bulk(arg.clone(), ini, vals);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = subst_bulk(dom.clone(), ini, vals);
      let img = subst_bulk(img.clone(), ini+1, vals);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = subst_bulk(typ.clone(), ini, vals);
      let exp = subst_bulk(exp.clone(), ini, vals);
      let bod = subst_bulk(bod.clone(), ini+1, vals);
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
    match &*node {
      Expr::App(fun, arg) => {
        args.push(arg.clone());
        node = fun.clone();
      }
      Expr::Lam(_, _, _, bod) if !args.is_empty() => {
        // Bulk substitution for efficiency
        let mut lams = 1;
        let mut bod = bod;
        loop {
          bod = match &**bod {
            Expr::Lam(_, _, _, bod) if args.len() > lams => {
              lams = lams + 1;
              bod
            },
            _ => {
              let subst_args = &args[args.len() - lams .. args.len()];
              node = subst_bulk(bod.clone(), 0, subst_args);
              args.truncate(args.len() - lams);
              break;
            },
          }
        }
      },
      Expr::Let(.., val, bod) => {
        subst(bod.clone(), 0, val.clone());
      },
      Expr::Sort(lvl) => {
        node = Rc::new(Expr::Sort(simplify(lvl)));
        break;
      },
      _ => break,
    }
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
    (Expr::Lam(a_nam, a_bnd, a_dom, a_bod), Expr::Lam(_, _, b_dom, b_bod)) => {
      if equal(a_dom, b_dom, unique) {
        let f_var = Rc::new(
          Expr::FVar(*unique, a_nam.clone(), *a_bnd, a_dom.clone())
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *unique = *unique + 1;
        return equal(&a_bod, &b_bod, unique)
      }
      false
    }
    (Expr::Pi(a_nam, a_bnd, a_dom, a_bod), Expr::Pi(_, _, b_dom, b_bod)) => {
      if equal(a_dom, b_dom, unique) {
        let f_var = Rc::new(
          Expr::FVar(*unique, a_nam.clone(), *a_bnd, a_dom.clone())
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *unique = *unique + 1;
        return equal(&a_bod, &b_bod, unique)
      }
      false
    }
    (Expr::FVar(uniq_a, ..), Expr::FVar(uniq_b, ..)) => {
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
    (Expr::Lam(a_nam, a_bnd, a_dom, a_bod), _) => {
      let f_var = Rc::new(
        Expr::FVar(*unique, a_nam.clone(), *a_bnd, a_dom.clone())
      );
      *unique = *unique + 1;
      let a_bod = subst(a_bod.clone(), 0, f_var.clone());
      let b_bod = Rc::new(Expr::App(fold_args(head_b, args_b), f_var));
      equal(&a_bod, &b_bod, unique)
    }
    (_, Expr::Lam(b_nam, b_bnd, b_dom, b_bod)) => {
      let f_var = Rc::new(
        Expr::FVar(*unique, b_nam.clone(), *b_bnd, b_dom.clone())
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

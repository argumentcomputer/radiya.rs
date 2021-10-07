use crate::{
  name::Name,
  universe::{
    Univ,
    simplify,
    is_same_level,
  },
  expression::Expr,
};

use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};
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

pub fn subst_lvl(vars: &Vector<Name>, map: &Vector<Univ>, lvl: &Rc<Univ>) -> Rc<Univ> {
  match &**lvl {
    Univ::Zero => lvl.clone(),
    Univ::Succ(pred) => {
      let pred = subst_lvl(vars, map, pred);
      Rc::new(Univ::Succ(pred))
    },
    Univ::Max(left, right) => {
      let left = subst_lvl(vars, map, left);
      let right = subst_lvl(vars, map, right);
      Rc::new(Univ::Max(left, right))
    },
    Univ::IMax(left, right) => {
      let left = subst_lvl(vars, map, left);
      let right = subst_lvl(vars, map, right);
      Rc::new(Univ::IMax(left, right))
    }
    Univ::Param(nam) => {
      match vars.iter().position(|var| var == nam) {
        Some(pos) => Rc::new(map[pos].clone()),
        None => lvl.clone(),
      }
    }
  }
}

// Analogously, assumes `vals` has no unbound `BVar` nodes
pub fn subst_bulk(bod: Rc<Expr>, ini: usize, vals: &[Rc<Expr>], u_vars: &Vector<Name>, u_map: &Vector<Univ>) -> Rc<Expr> {
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
      let bod = subst_bulk(bod.clone(), ini+1, vals, u_vars, u_map);
      let dom = subst_bulk(dom.clone(), ini, vals, u_vars, u_map);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = subst_bulk(fun.clone(), ini, vals, u_vars, u_map);
      let arg = subst_bulk(arg.clone(), ini, vals, u_vars, u_map);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = subst_bulk(dom.clone(), ini, vals, u_vars, u_map);
      let img = subst_bulk(img.clone(), ini+1, vals, u_vars, u_map);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = subst_bulk(typ.clone(), ini, vals, u_vars, u_map);
      let exp = subst_bulk(exp.clone(), ini, vals, u_vars, u_map);
      let bod = subst_bulk(bod.clone(), ini+1, vals, u_vars, u_map);
      Rc::new(Expr::Let(nam.clone(), typ, exp, bod))
    }
    Expr::Sort(lvl) if !u_vars.is_empty() => {
      let new_lvl = subst_lvl(u_vars, u_map, lvl);
      Rc::new(Expr::Sort(new_lvl))
    },
    _ => bod,
  }
}

pub fn whnf_unfold(
  tc : &TypeChecker,
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
              node = subst_bulk(bod.clone(), 0, subst_args, &Vector::new(), &Vector::new());
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
      Expr::Const(nam, lvls) => {
        let Recursor {
          name,
          uparams,
          num_params,
          num_motives,
          num_minors,
          num_indices,
          rec_rules,
          is_k,
          ..
        } = match tc.declars.get(&nam).expect("Undefined constant") {
          Declaration::Recursor(rec) => rec,
          Declaration::Quot {..} => todo!(),
          Declaration::Definition {..} => todo!(),
          _ => break,
        };

        let take_size = num_params + num_motives + num_minors;
        let major_idx = take_size + num_indices;
        if lvls.len() != uparams.len() || args.len() <= major_idx {
          break;
        }
        let major = &args[args.len() - 1 - major_idx];

        let (major_head, major_args) =
          if !is_k {
            whnf_unfold(tc, major.clone())
          } else {
            todo!()
          };

        let c_nam = match &*major_head {
          Expr::Const(c_nam, _) => c_nam.clone(),
          _ => break,
        };
        let rule = rec_rules.iter().find(
          |RecRule { cnstr_name, .. }|
          *cnstr_name == c_nam
        ).expect("Undefined recursion rule");
        if rule.elim_name != *name || major_args.len() != num_params + rule.num_fields {
          break
        }

        let mut subst = args[args.len() - take_size .. args.len()].to_vec();
        subst.extend_from_slice(&major_args[0 .. rule.num_fields]);
        let r = subst_bulk(rule.val.clone(), 0, &subst, uparams, lvls);
        node = r;
        args.truncate(args.len() - 1 - major_idx);
      }
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
  tc : &TypeChecker,
  node: Rc<Expr>,
) -> Rc<Expr> {
  let (head, args) = whnf_unfold(tc, node);
  fold_args(head, args)
}

pub fn equal(
  tc : &TypeChecker,
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
  let (head_a, args_a) = whnf_unfold(tc, expr_a.clone());
  let (head_b, args_b) = whnf_unfold(tc, expr_b.clone());
  match (&*head_a, &*head_b) {
    (Expr::Lam(a_nam, a_bnd, a_dom, a_bod), Expr::Lam(_, _, b_dom, b_bod)) => {
      if equal(tc, a_dom, b_dom, unique) {
        let f_var = Rc::new(
          Expr::FVar(*unique, a_nam.clone(), *a_bnd, a_dom.clone())
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *unique = *unique + 1;
        return equal(tc, &a_bod, &b_bod, unique)
      }
      false
    }
    (Expr::Pi(a_nam, a_bnd, a_dom, a_bod), Expr::Pi(_, _, b_dom, b_bod)) => {
      if equal(tc, a_dom, b_dom, unique) {
        let f_var = Rc::new(
          Expr::FVar(*unique, a_nam.clone(), *a_bnd, a_dom.clone())
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *unique = *unique + 1;
        return equal(tc, &a_bod, &b_bod, unique)
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
        return args_a.iter().zip(args_b).all(|(a, b)| equal(tc, &a, &b, unique))
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
      equal(tc, &a_bod, &b_bod, unique)
    }
    (_, Expr::Lam(b_nam, b_bnd, b_dom, b_bod)) => {
      let f_var = Rc::new(
        Expr::FVar(*unique, b_nam.clone(), *b_bnd, b_dom.clone())
      );
      *unique = *unique + 1;
      let b_bod = subst(b_bod.clone(), 0, f_var.clone());
      let a_bod = Rc::new(Expr::App(fold_args(head_a, args_a), f_var));
      equal(tc, &a_bod, &b_bod, unique)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReducibilityHint {
    Opaq,
    Reg(u16),
    Abbrev,
}

#[derive(Debug, Clone)]
pub struct RecRule {
    pub cnstr_name : Name,
    pub elim_name : Name,
    pub num_fields : usize,
    pub val : Rc<Expr>
}

#[derive(Debug, Clone)]
pub enum Declaration {
  Axiom {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
    is_unsafe : bool,
  },
  Definition {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
    val : Rc<Expr>,
    hint : ReducibilityHint,
    is_unsafe : bool,
  },
  Theorem {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
    val : Rc<Expr>,
  },
  Opaque {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
    val : Rc<Expr>,
  },
  Quot {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
  },
  Inductive {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
    num_params : u16,
    all_ind_names : Vector<Name>,
    all_cnstr_names : Vector<Name>,
    //pub is_rec : bool,
    //pub is_reflexive : bool,
    is_unsafe : bool,
  },
  Constructor {
    name : Name,
    uparams : Vector<Univ>,
    type_ : Rc<Expr>,
    parent_name : Name,
    num_fields : u16,
    num_params : u16,
    is_unsafe : bool,
  },
  Recursor(Recursor),
}

#[derive(Debug, Clone)]
pub struct Recursor {
  name : Name,
  uparams : Vector<Name>,
  type_ : Rc<Expr>,
  all_names : Vector<Name>,
  num_params : usize,
  num_indices : usize,
  num_motives : usize,
  num_minors : usize,
  major_idx : usize,
  rec_rules : Vector<RecRule>,
  is_k : bool,
  is_unsafe : bool,
}

pub struct TypeChecker {
  declars: BTreeMap<Name, Declaration>,
}

use crate::{
  name::Name,
  universe::{
    Univ,
    simplify,
    is_same_level,
  },
  expression::*,
};

use alloc::string::String;
use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};
use sp_im::vector::Vector;
use sp_std::rc::Rc;

pub fn abstr(bod: Rc<Expr>, dep: usize, idx: usize) -> Rc<Expr> {
  match &*bod {
    Expr::FVar(jdx, ..) => {
      if idx == *jdx {
        Rc::new(Expr::BVar(dep))
      }
      else {
        bod
      }
    }
    Expr::Lam(nam, bnd, dom, bod) => {
      let bod = abstr(bod.clone(), dep+1, idx);
      let dom = abstr(dom.clone(), dep, idx);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = abstr(fun.clone(), dep, idx);
      let arg = abstr(arg.clone(), dep, idx);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = abstr(dom.clone(), dep, idx);
      let img = abstr(img.clone(), dep+1, idx);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = abstr(typ.clone(), dep, idx);
      let exp = abstr(exp.clone(), dep, idx);
      let bod = abstr(bod.clone(), dep+1, idx);
      Rc::new(Expr::Let(nam.clone(), typ, exp, bod))
    }
    _ => bod,
  }
}

// Assumes `val` has no unbound `BVar` nodes; i.e., that they have been
// replaced by `FVar` nodes
pub fn subst(bod: Rc<Expr>, dep: usize, val: Rc<Expr>) -> Rc<Expr> {
  match &*bod {
    Expr::BVar(idx) => {
      if *idx < dep {
        bod
      }
      // This should always be the case if we are not reducing inner lambdas
      else if dep == *idx {
        val
      }
      else {
        Rc::new(Expr::BVar(*idx-1))
      }
    }
    Expr::Lam(nam, bnd, dom, bod) => {
      let bod = subst(bod.clone(), dep+1, val.clone());
      let dom = subst(dom.clone(), dep, val);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = subst(fun.clone(), dep, val.clone());
      let arg = subst(arg.clone(), dep, val);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = subst(dom.clone(), dep, val.clone());
      let img = subst(img.clone(), dep+1, val);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = subst(typ.clone(), dep, val.clone());
      let exp = subst(exp.clone(), dep, val.clone());
      let bod = subst(bod.clone(), dep+1, val);
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
pub fn subst_bulk(bod: Rc<Expr>, dep: usize, vals: &[Rc<Expr>], u_vars: &Vector<Name>, u_map: &Vector<Univ>) -> Rc<Expr> {
  match &*bod {
    Expr::BVar(idx) => {
      if *idx < dep {
        bod
      }
      // This should always be the case if we are not reducing inner lambdas
      else if *idx < dep+vals.len() {
        vals[idx-dep].clone()
      }
      else {
        Rc::new(Expr::BVar(*idx-vals.len()))
      }
    }
    Expr::Lam(nam, bnd, dom, bod) => {
      let bod = subst_bulk(bod.clone(), dep+1, vals, u_vars, u_map);
      let dom = subst_bulk(dom.clone(), dep, vals, u_vars, u_map);
      Rc::new(Expr::Lam(nam.clone(), *bnd, dom, bod))
    }
    Expr::App(fun, arg) => {
      let fun = subst_bulk(fun.clone(), dep, vals, u_vars, u_map);
      let arg = subst_bulk(arg.clone(), dep, vals, u_vars, u_map);
      Rc::new(Expr::App(fun, arg))
    }
    Expr::Pi(nam, bnd, dom, img) => {
      let dom = subst_bulk(dom.clone(), dep, vals, u_vars, u_map);
      let img = subst_bulk(img.clone(), dep+1, vals, u_vars, u_map);
      Rc::new(Expr::Pi(nam.clone(), *bnd, dom, img))
    }
    Expr::Let(nam, typ, exp, bod) => {
      let typ = subst_bulk(typ.clone(), dep, vals, u_vars, u_map);
      let exp = subst_bulk(exp.clone(), dep, vals, u_vars, u_map);
      let bod = subst_bulk(bod.clone(), dep+1, vals, u_vars, u_map);
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
  tc: &TypeChecker,
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
  tc: &TypeChecker,
  node: Rc<Expr>,
) -> Rc<Expr> {
  let (head, args) = whnf_unfold(tc, node);
  fold_args(head, args)
}

pub fn equal(
  tc: &TypeChecker,
  uniq: &mut usize,
  expr_a: &Rc<Expr>,
  expr_b: &Rc<Expr>,
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
      if equal(tc, uniq, a_dom, b_dom) {
        let f_var = Rc::new(
          Expr::FVar(*uniq, a_nam.clone(), *a_bnd, a_dom.clone())
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *uniq = *uniq + 1;
        return equal(tc, uniq, &a_bod, &b_bod)
      }
      false
    }
    (Expr::Pi(a_nam, a_bnd, a_dom, a_bod), Expr::Pi(_, _, b_dom, b_bod)) => {
      if equal(tc, uniq, a_dom, b_dom) {
        let f_var = Rc::new(
          Expr::FVar(*uniq, a_nam.clone(), *a_bnd, a_dom.clone())
        );
        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
        let b_bod = subst(b_bod.clone(), 0, f_var);
        *uniq = *uniq + 1;
        return equal(tc, uniq, &a_bod, &b_bod)
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
        return args_a.iter().zip(args_b).all(|(a, b)| equal(tc, uniq, &a, &b))
      }
      false
    }
    (Expr::Lam(a_nam, a_bnd, a_dom, a_bod), _) => {
      let f_var = Rc::new(
        Expr::FVar(*uniq, a_nam.clone(), *a_bnd, a_dom.clone())
      );
      *uniq = *uniq + 1;
      let a_bod = subst(a_bod.clone(), 0, f_var.clone());
      let b_bod = Rc::new(Expr::App(fold_args(head_b, args_b), f_var));
      equal(tc, uniq, &a_bod, &b_bod)
    }
    (_, Expr::Lam(b_nam, b_bnd, b_dom, b_bod)) => {
      let f_var = Rc::new(
        Expr::FVar(*uniq, b_nam.clone(), *b_bnd, b_dom.clone())
      );
      *uniq = *uniq + 1;
      let b_bod = subst(b_bod.clone(), 0, f_var.clone());
      let a_bod = Rc::new(Expr::App(fold_args(head_a, args_a), f_var));
      equal(tc, uniq, &a_bod, &b_bod)
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

#[derive(Debug)]
pub enum CheckError {
  GenericError(String)
}

pub fn check(tc: &TypeChecker, uniq: &mut usize, term: &Rc<Expr>, typ: &Rc<Expr>) -> Result<(), CheckError> {
  let inferred = infer(tc, uniq, term)?;
  if equal(tc, uniq, typ, &inferred) {
    Ok(())
  }
  else {
    Err(CheckError::GenericError(format!("Wrong type")))
  }
}

pub fn infer(
  tc: &TypeChecker,
  uniq: &mut usize,
  term : &Rc<Expr>
) -> Result<Rc<Expr>, CheckError> {
  let result = match &**term {
    Expr::Sort(lvl) => {
      let new_lvl = Rc::new(Expr::Sort(Rc::new(Univ::Succ(lvl.clone()))));
      Ok(new_lvl)
    },
    Expr::Const(name, lvls) => infer_const(name, lvls),
    Expr::FVar(.., typ) => Ok(typ.clone()),
    Expr::App(fun, arg) => infer_app(tc, uniq, fun, arg),
    Expr::Lam(nam, bnd, dom, bod) => infer_lambda(tc, uniq, nam, bnd, dom, bod),
    Expr::Pi(nam, bnd, dom, img) => infer_pi(tc, uniq, nam, bnd, dom, img),
    Expr::Let(_, dom, val, body) => infer_let(tc, uniq, dom, val, body),
    other => {
      let msg = format!(
        "tc line {}; infer function got a variable term, but that should never happen. received this term : {:?}\n",
        line!(), other
      );
      Err(CheckError::GenericError(msg))
    },
  };
  result
}

#[inline]
pub fn infer_universe_of_type(tc: &TypeChecker, uniq: &mut usize, term : &Rc<Expr>) -> Result<Rc<Univ>, CheckError> {
  let typ = infer(tc, uniq, term)?;
  match &*whnf(tc, typ) {
    Expr::Sort(lvl) => Ok(lvl.clone()),
    _ => {
      let msg = format!("Domain of the function should be a type");
      return Err(CheckError::GenericError(msg))
    }
  }
}

#[inline]
pub fn infer_lambda(
  tc: &TypeChecker,
  uniq: &mut usize,
  nam: &Name,
  bnd: &BinderInfo,
  dom: &Rc<Expr>,
  bod: &Rc<Expr>
) -> Result<Rc<Expr>, CheckError> {
  infer_universe_of_type(tc, uniq, dom)?;
  let new_var = Rc::new(Expr::FVar(*uniq, nam.clone(), bnd.clone(), dom.clone()));
  let new_var_idx = *uniq;
  *uniq = *uniq + 1;
  let new_bod = subst(bod.clone(), 0, new_var);
  let infer_typ_bod = infer(tc, uniq, &new_bod)?;
  let infer_typ_bod = abstr(infer_typ_bod, 0, new_var_idx);
  Ok(Rc::new(Expr::Lam(nam.clone(), bnd.clone(), dom.clone(), infer_typ_bod)))
}

#[inline]
fn infer_app(
  tc: &TypeChecker,
  uniq: &mut usize,
  fun: &Rc<Expr>,
  arg: &Rc<Expr>
) -> Result<Rc<Expr>, CheckError> {
  let fun_typ = infer(tc, uniq, &fun)?;
  match &*whnf(tc, fun_typ) {
    Expr::Pi(_, _, dom, img) => {
      check(tc, uniq, &arg, &dom)?;
      let app_typ = subst(img.clone(), 0, arg.clone());
      Ok(app_typ)
    }
    _ => Err(CheckError::GenericError(
      format!("Tried to apply an expression which is not a function")
    )),
  }
}

#[inline]
pub fn infer_pi(
  tc: &TypeChecker,
  uniq: &mut usize,
  nam: &Name,
  bnd: &BinderInfo,
  dom: &Rc<Expr>,
  img: &Rc<Expr>
) -> Result<Rc<Expr>, CheckError> {
  let dom_lvl = infer_universe_of_type(tc, uniq, dom)?;
  let new_var = Rc::new(Expr::FVar(*uniq, nam.clone(), bnd.clone(), dom.clone()));
  *uniq = *uniq + 1;
  let new_img = subst(img.clone(), 0, new_var);
  let img_lvl = infer_universe_of_type(tc, uniq, &new_img)?;
  let pi_lvl = Rc::new(Univ::IMax(dom_lvl, img_lvl));
  Ok(Rc::new(Expr::Sort(pi_lvl)))
}

#[inline]
pub fn infer_const(name : &Name, levels : &Vector<Univ>) -> Result<Rc<Expr>, CheckError> {
  todo!()
}

#[inline]
pub fn infer_let(tc: &TypeChecker, uniq: &mut usize, dom : &Rc<Expr>, val : &Rc<Expr>, body : &Rc<Expr>) -> Result<Rc<Expr>, CheckError> {
  infer_universe_of_type(tc, uniq, dom)?;
  check(tc, uniq, val, dom)?;
  let instd_body = subst(body.clone(), 0, val.clone());
  infer(tc, uniq, &instd_body)
}

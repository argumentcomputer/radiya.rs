use crate::{
  expression::*,
  name::Name,
  universe::{
    is_same_level,
    simplify,
    Universe,
  },
};

use alloc::string::String;
use sp_im::vector::Vector;
use sp_std::{
  collections::btree_map::BTreeMap,
  rc::Rc,
  vec::Vec,
};

// pub fn abstr(bod: Rc<Expression>, dep: usize, idx: usize) -> Rc<Expression> {
//  match &*bod {
//    Expression::FVar(_, jdx, ..) => {
//      if idx == *jdx {
//        Rc::new(Expression::BVar(None, dep))
//      }
//      else {
//        bod
//      }
//    }
//    Expression::Lam(_, nam, bnd, dom, bod) => {
//      let bod = abstr(bod.clone(), dep + 1, idx);
//      let dom = abstr(dom.clone(), dep, idx);
//      Rc::new(Expression::Lam(None, nam.clone(), *bnd, dom, bod))
//    }
//    Expression::App(_, fun, arg) => {
//      let fun = abstr(fun.clone(), dep, idx);
//      let arg = abstr(arg.clone(), dep, idx);
//      Rc::new(Expression::App(None, fun, arg))
//    }
//    Expression::Pi(_, nam, bnd, dom, img) => {
//      let dom = abstr(dom.clone(), dep, idx);
//      let img = abstr(img.clone(), dep + 1, idx);
//      Rc::new(Expression::Pi(None, nam.clone(), *bnd, dom, img))
//    }
//    Expression::Let(_, nam, typ, exp, bod) => {
//      let typ = abstr(typ.clone(), dep, idx);
//      let exp = abstr(exp.clone(), dep, idx);
//      let bod = abstr(bod.clone(), dep + 1, idx);
//      Rc::new(Expression::Let(None, nam.clone(), typ, exp, bod))
//    }
//    _ => bod,
//  }
//}
//
//// Assumes `val` has no unbound `BVar` nodes; i.e., that they have been
//// replaced by `FVar` nodes
// pub fn subst(
//  bod: Rc<Expression>,
//  dep: usize,
//  val: Rc<Expression>,
//) -> Rc<Expression> {
//  match &*bod {
//    Expression::BVar(_, idx) => {
//      if *idx < dep {
//        bod
//      }
//      // This should always be the case if we are not reducing inner lambdas
//      else if dep == *idx {
//        val
//      }
//      else {
//        Rc::new(Expression::BVar(None, *idx - 1))
//      }
//    }
//    Expression::Lam(_, nam, bnd, dom, bod) => {
//      let bod = subst(bod.clone(), dep + 1, val.clone());
//      let dom = subst(dom.clone(), dep, val);
//      Rc::new(Expression::Lam(None, nam.clone(), *bnd, dom, bod))
//    }
//    Expression::App(None, fun, arg) => {
//      let fun = subst(fun.clone(), dep, val.clone());
//      let arg = subst(arg.clone(), dep, val);
//      Rc::new(Expression::App(None, fun, arg))
//    }
//    Expression::Pi(None, nam, bnd, dom, img) => {
//      let dom = subst(dom.clone(), dep, val.clone());
//      let img = subst(img.clone(), dep + 1, val);
//      Rc::new(Expression::Pi(None, nam.clone(), *bnd, dom, img))
//    }
//    Expression::Let(None, nam, typ, exp, bod) => {
//      let typ = subst(typ.clone(), dep, val.clone());
//      let exp = subst(exp.clone(), dep, val.clone());
//      let bod = subst(bod.clone(), dep + 1, val);
//      Rc::new(Expression::Let(None, nam.clone(), typ, exp, bod))
//    }
//    _ => bod,
//  }
//}
// pub fn subst_lvl(
//  vars: &Vector<Name>,
//  map: &Vector<Universe>,
//  lvl: &Rc<Universe>,
//) -> Rc<Universe> {
//  match &**lvl {
//    Universe::Zero => lvl.clone(),
//    Universe::Succ(pred) => {
//      let pred = subst_lvl(vars, map, pred);
//      Rc::new(Universe::Succ(pred))
//    }
//    Universe::Max(left, right) => {
//      let left = subst_lvl(vars, map, left);
//      let right = subst_lvl(vars, map, right);
//      Rc::new(Universe::Max(left, right))
//    }
//    Universe::IMax(left, right) => {
//      let left = subst_lvl(vars, map, left);
//      let right = subst_lvl(vars, map, right);
//      Rc::new(Universe::IMax(left, right))
//    }
//    Universe::Param(nam, idx) => match vars.iter().position(|var| var == nam)
// {      Some(pos) => Rc::new(map[pos].clone()),
//      None => lvl.clone(),
//    },
//  }
//}
//
//// Analogously, assumes `vals` has no unbound `BVar` nodes
// pub fn subst_bulk(
//  bod: Rc<Expression>,
//  dep: usize,
//  vals: &[Rc<Expression>],
//  u_vars: &Vector<Name>,
//  u_map: &Vector<Universe>,
//) -> Rc<Expression> {
//  match &*bod {
//    Expression::BVar(_, idx) => {
//      if *idx < dep {
//        bod
//      }
//      // This should always be the case if we are not reducing inner lambdas
//      else if *idx < dep + vals.len() {
//        vals[idx - dep].clone()
//      }
//      else {
//        Rc::new(Expression::BVar(None, *idx - vals.len()))
//      }
//    }
//    Expression::Lam(_, nam, bnd, dom, bod) => {
//      let bod = subst_bulk(bod.clone(), dep + 1, vals, u_vars, u_map);
//      let dom = subst_bulk(dom.clone(), dep, vals, u_vars, u_map);
//      Rc::new(Expression::Lam(None, nam.clone(), *bnd, dom, bod))
//    }
//    Expression::App(_, fun, arg) => {
//      let fun = subst_bulk(fun.clone(), dep, vals, u_vars, u_map);
//      let arg = subst_bulk(arg.clone(), dep, vals, u_vars, u_map);
//      Rc::new(Expression::App(None, fun, arg))
//    }
//    Expression::Pi(_, nam, bnd, dom, img) => {
//      let dom = subst_bulk(dom.clone(), dep, vals, u_vars, u_map);
//      let img = subst_bulk(img.clone(), dep + 1, vals, u_vars, u_map);
//      Rc::new(Expression::Pi(None, nam.clone(), *bnd, dom, img))
//    }
//    Expression::Let(_, nam, typ, exp, bod) => {
//      let typ = subst_bulk(typ.clone(), dep, vals, u_vars, u_map);
//      let exp = subst_bulk(exp.clone(), dep, vals, u_vars, u_map);
//      let bod = subst_bulk(bod.clone(), dep + 1, vals, u_vars, u_map);
//      Rc::new(Expression::Let(None, nam.clone(), typ, exp, bod))
//    }
//    Expression::Sort(_, lvl) if !u_vars.is_empty() => {
//      let new_lvl = subst_lvl(u_vars, u_map, lvl);
//      Rc::new(Expression::Sort(None, new_lvl))
//    }
//    _ => bod,
//  }
//}
// pub fn whnf_unfold(
//  tc: &TypeChecker,
//  top_node: Rc<Expression>,
//) -> (Rc<Expression>, Vec<Rc<Expression>>) {
//  let mut node = top_node;
//  let mut args = Vec::new();
//  loop {
//    match &*node {
//      Expression::App(_, fun, arg) => {
//        args.push(arg.clone());
//        node = fun.clone();
//      }
//      Expression::Lam(_, _, _, _, bod) if !args.is_empty() => {
//        // Bulk substitution for efficiency
//        let mut lams = 1;
//        let mut bod = bod;
//        loop {
//          bod = match &**bod {
//            Expression::Lam(_, _, _, _, bod) if args.len() > lams => {
//              lams = lams + 1;
//              bod
//            }
//            _ => {
//              let subst_args = &args[args.len() - lams..args.len()];
//              node = subst_bulk(
//                bod.clone(),
//                0,
//                subst_args,
//                &Vector::new(),
//                &Vector::new(),
//              );
//              args.truncate(args.len() - lams);
//              break;
//            }
//          }
//        }
//      }
//      Expression::Let(_, _, _, val, bod) => {
//        subst(bod.clone(), 0, val.clone());
//      }
//      Expression::Sort(_, lvl) => {
//        node = Rc::new(Expression::Sort(None, simplify(lvl)));
//        break;
//      }
//      Expression::Const(_, nam, lvls) => {
//        let Declaration { uparams, which, .. } =
//          tc.declars.get(&nam).expect("Undefined constant");
//        match which {
//          DeclarationParticular::Recursor {
//            num_params,
//            num_motives,
//            num_minors,
//            num_indices,
//            rec_rules,
//            is_k,
//            ..
//          } => {
//            let take_size = num_params + num_motives + num_minors;
//            let major_idx = take_size + num_indices;
//            if lvls.len() != uparams.len() || args.len() <= major_idx {
//              break;
//            }
//            let major = &args[args.len() - 1 - major_idx];
//
//            let (major_head, major_args) =
//              if !is_k { whnf_unfold(tc, major.clone()) } else { todo!() };
//
//            let rule = match &*major_head {
//              Expression::Const(_, c_nam, _) => {
//                rec_rules.get(c_nam).expect("Undefined recursion rule")
//              }
//              _ => break,
//            };
//            if rule.elim_name != *nam
//              || major_args.len() != num_params + rule.num_fields
//            {
//              break;
//            }
//
//            let mut subst = args[args.len() - take_size..args.len()].to_vec();
//            subst.extend_from_slice(&major_args[0..rule.num_fields]);
//            let r = subst_bulk(rule.val.clone(), 0, &subst, uparams, lvls);
//            node = r;
//            args.truncate(args.len() - 1 - major_idx);
//          }
//          DeclarationParticular::Definition { .. } => todo!(),
//          _ => break,
//        }
//      }
//      _ => break,
//    }
//  }
//  (node, args)
//}
//
//#[inline]
// pub fn fold_args(
//  mut head: Rc<Expression>,
//  mut args: Vec<Rc<Expression>>,
//) -> Rc<Expression> {
//  while let Some(arg) = args.pop() {
//    head = Rc::new(Expression::App(None, head, arg.clone()));
//  }
//  head
//}
//
//#[inline]
// pub fn whnf(tc: &TypeChecker, node: Rc<Expression>) -> Rc<Expression> {
//  let (head, args) = whnf_unfold(tc, node);
//  fold_args(head, args)
//}
// pub fn equal(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  expr_a: &Rc<Expression>,
//  expr_b: &Rc<Expression>,
//) -> bool {
//  // Short circuit if they're the same pointer
//  if Rc::as_ptr(expr_a) == Rc::as_ptr(expr_b) {
//    return true;
//  }
//  // Return true if they are proof irrelevant terms
//  if is_proof_irrel(expr_a, expr_b) {
//    return true;
//  }
//  let (head_a, args_a) = whnf_unfold(tc, expr_a.clone());
//  let (head_b, args_b) = whnf_unfold(tc, expr_b.clone());
//  match (&*head_a, &*head_b) {
//    (
//      Expression::Lam(_, a_nam, a_bnd, a_dom, a_bod),
//      Expression::Lam(_, _, _, b_dom, b_bod),
//    ) => {
//      if equal(tc, uniq, a_dom, b_dom) {
//        let f_var = Rc::new(Expression::FVar(
//          None,
//          *uniq,
//          a_nam.clone(),
//          *a_bnd,
//          a_dom.clone(),
//        ));
//        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
//        let b_bod = subst(b_bod.clone(), 0, f_var);
//        *uniq = *uniq + 1;
//        return equal(tc, uniq, &a_bod, &b_bod);
//      }
//      false
//    }
//    (
//      Expression::Pi(_, a_nam, a_bnd, a_dom, a_bod),
//      Expression::Pi(_, _, _, b_dom, b_bod),
//    ) => {
//      if equal(tc, uniq, a_dom, b_dom) {
//        let f_var = Rc::new(Expression::FVar(
//          None,
//          *uniq,
//          a_nam.clone(),
//          *a_bnd,
//          a_dom.clone(),
//        ));
//        let a_bod = subst(a_bod.clone(), 0, f_var.clone());
//        let b_bod = subst(b_bod.clone(), 0, f_var);
//        *uniq = *uniq + 1;
//        return equal(tc, uniq, &a_bod, &b_bod);
//      }
//      false
//    }
//    (Expression::FVar(_, uniq_a, ..), Expression::FVar(_, uniq_b, ..)) => {
//      *uniq_a == *uniq_b
//    }
//    (Expression::Sort(_, lvl_a), Expression::Sort(_, lvl_b)) => {
//      is_same_level(lvl_a, lvl_b)
//    }
//    (
//      Expression::Const(_, nam_a, lvls_a),
//      Expression::Const(_, nam_b, lvls_b),
//    ) => {
//      if nam_a == nam_b
//        && lvls_a.iter().zip(lvls_b).all(|(a, b)| is_same_level(a, b))
//      {
//        return args_a.iter().zip(args_b).all(|(a, b)| equal(tc, uniq, &a,
// &b));      }
//      false
//    }
//    (Expression::Lam(_, a_nam, a_bnd, a_dom, a_bod), _) => {
//      let f_var = Rc::new(Expression::FVar(
//        None,
//        *uniq,
//        a_nam.clone(),
//        *a_bnd,
//        a_dom.clone(),
//      ));
//      *uniq = *uniq + 1;
//      let a_bod = subst(a_bod.clone(), 0, f_var.clone());
//      let b_bod =
//        Rc::new(Expression::App(None, fold_args(head_b, args_b), f_var));
//      equal(tc, uniq, &a_bod, &b_bod)
//    }
//    (_, Expression::Lam(None, b_nam, b_bnd, b_dom, b_bod)) => {
//      let f_var = Rc::new(Expression::FVar(
//        None,
//        *uniq,
//        b_nam.clone(),
//        *b_bnd,
//        b_dom.clone(),
//      ));
//      *uniq = *uniq + 1;
//      let b_bod = subst(b_bod.clone(), 0, f_var.clone());
//      let a_bod =
//        Rc::new(Expression::App(None, fold_args(head_a, args_a), f_var));
//      equal(tc, uniq, &a_bod, &b_bod)
//    }
//    _ => false,
//  }
//}
// pub fn is_proof_irrel(
//  _expr_a: &Rc<Expression>,
//  _expr_b: &Rc<Expression>,
//) -> bool {
//  // TODO
//  false
//}
//
//#[derive(Debug, Clone)]
// pub struct RecRule {
//  pub elim_name: Name,
//  pub num_fields: usize,
//  pub val: Rc<Expression>,
//}
// pub struct Declaration {
//  uparams: Vector<Name>,
//  type_: Rc<Expression>,
//  // is_unsafe: bool,
//  which: DeclarationParticular,
//}
//
//#[derive(Debug, Clone)]
// pub enum DeclarationParticular {
//  // Constant representing an irreducible primitive of a certain type.
//  // Introduced by keyword `axiom` or `constant`
//  Axiom,
//  // Constant that binds a name to an expression of a certain type. Introduced
//  // by keyword `def`
//  Definition {
//    val: Rc<Expression>,
//  },
//  // Similar to definition, but used for propositions instead of types.
//  // Introduced by keyword `theorem` or `lemma`
//  Theorem {
//    val: Rc<Expression>,
//  },
//  // Constant representing an inductive datatype. Introduced by keyword
//  // `inductive`
//  Inductive {
//    num_params: u16,
//    all_ind_names: Vector<Name>,
//    all_cnstr_names: Vector<Name>,
//    /* pub is_rec: bool,
//     * pub is_reflexive: bool, */
//  },
//  // Constant representing a constructor of a inductive datatype. Introduced
//  // by keyword `inductive`
//  Constructor {
//    parent_name: Name,
//    num_fields: u16,
//    num_params: u16,
//  },
//  // Constant representing the recursor of a inductive datatype. Introduced by
//  // keyword `inductive`
//  Recursor {
//    all_names: Vector<Name>,
//    num_params: usize,
//    num_indices: usize,
//    num_motives: usize,
//    num_minors: usize,
//    major_idx: usize,
//    rec_rules: BTreeMap<Name, RecRule>,
//    is_k: bool,
//  },
//}
// pub struct TypeChecker {
//  declars: BTreeMap<Name, Declaration>,
//}
//
//#[derive(Debug)]
// pub enum CheckError {
//  GenericError(String),
//}
// pub fn check(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  term: &Rc<Expression>,
//  typ: &Rc<Expression>,
//) -> Result<(), CheckError> {
//  let inferred = infer(tc, uniq, term)?;
//  if equal(tc, uniq, typ, &inferred) {
//    Ok(())
//  }
//  else {
//    Err(CheckError::GenericError(format!("Wrong type")))
//  }
//}
// pub fn infer(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  term: &Rc<Expression>,
//) -> Result<Rc<Expression>, CheckError> {
//  let result = match &**term {
//    Expression::Sort(_, lvl) => {
//      let new_lvl =
//        Rc::new(Expression::Sort(None, Rc::new(Universe::Succ(lvl.clone()))));
//      Ok(new_lvl)
//    }
//    Expression::Const(_, name, lvls) => infer_const(tc, name, lvls),
//    Expression::FVar(.., typ) => Ok(typ.clone()),
//    Expression::App(_, fun, arg) => infer_app(tc, uniq, fun, arg),
//    Expression::Lam(_, nam, bnd, dom, bod) => {
//      infer_lambda(tc, uniq, nam, bnd, dom, bod)
//    }
//    Expression::Pi(_, nam, bnd, dom, img) => {
//      infer_pi(tc, uniq, nam, bnd, dom, img)
//    }
//    Expression::Let(_, _, dom, val, body) => {
//      infer_let(tc, uniq, dom, val, body)
//    }
//    other => {
//      let msg = format!(
//        "tc line {}; infer function got a variable term, but that should \
//         never happen. received this term : {:?}\n",
//        line!(),
//        other
//      );
//      Err(CheckError::GenericError(msg))
//    }
//  };
//  result
//}
//
//#[inline]
// pub fn infer_universe_of_type(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  term: &Rc<Expression>,
//) -> Result<Rc<Universe>, CheckError> {
//  let typ = infer(tc, uniq, term)?;
//  match &*whnf(tc, typ) {
//    Expression::Sort(_, lvl) => Ok(lvl.clone()),
//    _ => {
//      let msg = format!("Domain of the function should be a type");
//      return Err(CheckError::GenericError(msg));
//    }
//  }
//}
//
//#[inline]
// pub fn infer_lambda(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  nam: &Name,
//  bnd: &BinderInfo,
//  dom: &Rc<Expression>,
//  bod: &Rc<Expression>,
//) -> Result<Rc<Expression>, CheckError> {
//  infer_universe_of_type(tc, uniq, dom)?;
//  let new_var = Rc::new(Expression::FVar(
//    None,
//    *uniq,
//    nam.clone(),
//    bnd.clone(),
//    dom.clone(),
//  ));
//  let new_var_idx = *uniq;
//  *uniq = *uniq + 1;
//  let new_bod = subst(bod.clone(), 0, new_var);
//  let infer_typ_bod = infer(tc, uniq, &new_bod)?;
//  let infer_typ_bod = abstr(infer_typ_bod, 0, new_var_idx);
//  Ok(Rc::new(Expression::Lam(
//    None,
//    nam.clone(),
//    bnd.clone(),
//    dom.clone(),
//    infer_typ_bod,
//  )))
//}
//
//#[inline]
// fn infer_app(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  fun: &Rc<Expression>,
//  arg: &Rc<Expression>,
//) -> Result<Rc<Expression>, CheckError> {
//  let fun_typ = infer(tc, uniq, &fun)?;
//  match &*whnf(tc, fun_typ) {
//    Expression::Pi(_, _, _, dom, img) => {
//      check(tc, uniq, &arg, &dom)?;
//      let app_typ = subst(img.clone(), 0, arg.clone());
//      Ok(app_typ)
//    }
//    _ => Err(CheckError::GenericError(format!(
//      "Tried to apply an expression which is not a function"
//    ))),
//  }
//}
//
//#[inline]
// pub fn infer_pi(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  nam: &Name,
//  bnd: &BinderInfo,
//  dom: &Rc<Expression>,
//  img: &Rc<Expression>,
//) -> Result<Rc<Expression>, CheckError> {
//  let dom_lvl = infer_universe_of_type(tc, uniq, dom)?;
//  let new_var = Rc::new(Expression::FVar(
//    None,
//    *uniq,
//    nam.clone(),
//    bnd.clone(),
//    dom.clone(),
//  ));
//  *uniq = *uniq + 1;
//  let new_img = subst(img.clone(), 0, new_var);
//  let img_lvl = infer_universe_of_type(tc, uniq, &new_img)?;
//  let pi_lvl = Rc::new(Universe::IMax(dom_lvl, img_lvl));
//  Ok(Rc::new(Expression::Sort(None, pi_lvl)))
//}
//
//#[inline]
// pub fn infer_const(
//  tc: &TypeChecker,
//  nam: &Name,
//  levels: &Vector<Universe>,
//) -> Result<Rc<Expression>, CheckError> {
//  let cnst = tc.declars.get(&nam).expect("Undefined constant");
//  if levels.len() == cnst.uparams.len() {
//    Ok(cnst.type_.clone())
//  }
//  else {
//    Err(CheckError::GenericError(format!(
//      "Constant has wrong number of levels"
//    )))
//  }
//}
//
//#[inline]
// pub fn infer_let(
//  tc: &TypeChecker,
//  uniq: &mut usize,
//  dom: &Rc<Expression>,
//  val: &Rc<Expression>,
//  body: &Rc<Expression>,
//) -> Result<Rc<Expression>, CheckError> {
//  infer_universe_of_type(tc, uniq, dom)?;
//  check(tc, uniq, val, dom)?;
//  let instd_body = subst(body.clone(), 0, val.clone());
//  infer(tc, uniq, &instd_body)
//}

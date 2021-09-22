use crate::expression::BinderInfo;

use num_bigint::BigUint;
use sp_im::vector::Vector;

use sp_std::{
  collections::btree_map::BTreeMap,
  rc::Rc,
  vec::Vec,
};

use crate::{
  expression,
  name,
  universe,
};

use alloc::string::String;

// universe index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct UIdx(pub u64);

// expr index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct EIdx(pub u64);

// name index
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct NIdx(pub u64);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Name {
  /// Anonymous name
  Anon,
  /// Extends a hierarchical name `prev` with `name` to make `prev.name`
  Str { prev: NIdx, name: String },
  /// Extends a hierarchical name `prev` with `int` to make `prev.int`
  Int { prev: NIdx, int: BigUint },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Univ {
  /// The 0 universe
  Zero,
  /// Defines the successor universe of pred
  Succ { pred: UIdx },
  /// Defines the maximum universe of the lhs and rhs:
  Max { lhs: UIdx, rhs: UIdx },
  /// Defines the impredicative maximum universe for lhs and
  /// rhs, which is zero if rhs is zero and #UM otherwise.
  IMax { lhs: UIdx, rhs: UIdx },
  /// Defines a universe parameter
  Param { name: NIdx },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  /// #EV
  Var { idx: u64 },
  /// #ES
  Sort { univ: UIdx },
  /// #EC
  Const { name: NIdx, levels: Vector<UIdx> },
  /// #EA
  App { fun: EIdx, arg: EIdx },
  /// #EL
  Lam { info: BinderInfo, name: NIdx, typ: EIdx, bod: EIdx },
  /// #EP
  Pi { info: BinderInfo, name: NIdx, typ: EIdx, bod: EIdx },
  /// #EZ
  Let { name: NIdx, typ: EIdx, val: EIdx, bod: EIdx },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
  /// #DEF
  Definition { name: NIdx, typ: EIdx, val: EIdx, levels: Vector<NIdx> },
  /// #IND
  Inductive {
    num_params: u64,
    name: NIdx,
    typ: EIdx,
    intros: Vector<(NIdx, EIdx)>,
    levels: Vector<NIdx>,
  },
  /// #AX
  Axiom { name: NIdx, typ: EIdx, levels: Vector<NIdx> },
  /// #QUOT
  Quotient,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Notation {
  Prefix { name: NIdx, prec: u64, token: String },
  Infix { name: NIdx, prec: u64, token: String },
  Postfix { name: NIdx, prec: u64, token: String },
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Ctx {
  pub univs: BTreeMap<UIdx, Univ>,
  pub names: BTreeMap<NIdx, Name>,
  pub exprs: BTreeMap<EIdx, Expr>,
  pub notations: Vec<Notation>,
  pub decls: Vec<Decl>,
}

impl Ctx {
  pub fn new() -> Self {
    Ctx {
      univs: vec![(UIdx(0), Univ::Zero)].into_iter().collect(),
      names: vec![(NIdx(0), Name::Anon)].into_iter().collect(),
      exprs: BTreeMap::new(),
      notations: Vec::new(),
      decls: Vec::new(),
    }
  }

  pub fn make_names(&self) -> BTreeMap<NIdx, name::Name> {
    let mut names = BTreeMap::new();
    for (nidx, name) in self.names.iter() {
      match name {
        Name::Anon => {
          names.insert(*nidx, name::Name { parts: Vector::new() });
        }
        Name::Str { prev, name } => {
          let mut parts =
            names.get(prev).expect("Ctx indices not in order").parts.clone();
          parts.push_back(name::NamePart::Str(name.clone()));
          names.insert(*nidx, name::Name { parts });
        }
        Name::Int { prev, int } => {
          let mut parts =
            names.get(prev).expect("Ctx indices not in order").parts.clone();
          parts.push_back(name::NamePart::Int(int.clone()));
          names.insert(*nidx, name::Name { parts });
        }
      }
    }
    names
  }

  pub fn make_univs(
    &self,
    names: &BTreeMap<NIdx, name::Name>,
  ) -> BTreeMap<UIdx, Rc<universe::Univ>> {
    let mut univs = BTreeMap::new();
    for (uidx, univ) in self.univs.iter() {
      match univ {
        Univ::Zero => {
          univs.insert(*uidx, Rc::new(universe::Univ::Zero));
        }
        Univ::Succ { pred } => {
          let pred = univs.get(pred).expect("Ctx indices not in order").clone();
          univs.insert(*uidx, Rc::new(universe::Univ::Succ(pred)));
        }
        Univ::Max { lhs, rhs } => {
          let lhs = univs.get(lhs).expect("Ctx indices not in order").clone();
          let rhs = univs.get(rhs).expect("Ctx indices not in order").clone();
          univs.insert(*uidx, Rc::new(universe::Univ::Max(lhs, rhs)));
        }
        Univ::IMax { lhs, rhs } => {
          let lhs = univs.get(lhs).expect("Ctx indices not in order").clone();
          let rhs = univs.get(rhs).expect("Ctx indices not in order").clone();
          univs.insert(*uidx, Rc::new(universe::Univ::IMax(lhs, rhs)));
        }
        Univ::Param { name } => {
          let name: name::Name =
            names.get(name).expect("Ctx indices not in order").clone();
          univs.insert(*uidx, Rc::new(universe::Univ::Param(name)));
        }
      }
    }
    univs
  }

  pub fn make_exprs(
    &self,
    names: &BTreeMap<NIdx, name::Name>,
    univs: &BTreeMap<UIdx, Rc<universe::Univ>>,
  ) -> BTreeMap<EIdx, Rc<expression::Expr>> {
    let mut exprs = BTreeMap::new();
    for (eidx, expr) in self.exprs.iter() {
      match expr {
        Expr::Var { idx } => {
          exprs.insert(*eidx, Rc::new(expression::Expr::BVar(*idx as usize)));
        }
        Expr::Sort { univ } => {
          let univ = univs.get(univ).expect("Ctx indices not in order").clone();
          exprs.insert(*eidx, Rc::new(expression::Expr::Sort(univ)));
        }
        Expr::Const { name, levels } => {
          let name = names.get(name).expect("Ctx indices not in order").clone();
          let mut ls = Vector::new();
          for level in levels {
            ls.push_back(
              univs.get(level).expect("Ctx indices not in order").clone(),
            )
          }
          exprs.insert(*eidx, Rc::new(expression::Expr::Const(name, ls)));
        }
        Expr::App { fun, arg } => {
          let fun = exprs.get(fun).expect("Ctx indices not in order").clone();
          let arg = exprs.get(arg).expect("Ctx indices not in order").clone();
          exprs.insert(*eidx, Rc::new(expression::Expr::App(fun, arg)));
        }
        Expr::Lam { info, name, typ, bod } => {
          let nam = names.get(name).expect("Ctx indices not in order").clone();
          let typ = exprs.get(typ).expect("Ctx indices not in order").clone();
          let bod = exprs.get(bod).expect("Ctx indices not in order").clone();
          exprs.insert(
            *eidx,
            Rc::new(expression::Expr::Lam(nam, *info, typ, bod)),
          );
        }
        Expr::Pi { info, name, typ, bod } => {
          let nam = names.get(name).expect("Ctx indices not in order").clone();
          let typ = exprs.get(typ).expect("Ctx indices not in order").clone();
          let bod = exprs.get(bod).expect("Ctx indices not in order").clone();
          exprs
            .insert(*eidx, Rc::new(expression::Expr::Pi(nam, *info, typ, bod)));
        }
        Expr::Let { name, typ, val, bod } => {
          let nam = names.get(name).expect("Ctx indices not in order").clone();
          let typ = exprs.get(typ).expect("Ctx indices not in order").clone();
          let val = exprs.get(val).expect("Ctx indices not in order").clone();
          let bod = exprs.get(bod).expect("Ctx indices not in order").clone();
          exprs
            .insert(*eidx, Rc::new(expression::Expr::Let(nam, typ, val, bod)));
        }
      }
    }
    exprs
  }
}

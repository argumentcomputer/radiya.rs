use sp_std::{
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use num_bigint::BigUint;

use alloc::string::String;

use crate::{
  declaration::{
    ConstantInfo,
    Declaration,
  },
  export::Notation,
  expression::Expr,
  local_context::LocalContext,
  name::Name,
};

pub struct Environment {
  pub constants: BTreeMap<Name, ConstantInfo>,
  pub notations: BTreeMap<Name, Notation>,
  pub quot_init: bool,
}

impl Environment {
  pub fn get(&self, name: &Name) -> Option<&ConstantInfo> {
    self.constants.get(name)
  }
}

pub enum KernelException {
  UnknownConstant {
    env: Environment,
    name: Name,
  },
  AlreadyDeclared {
    env: Environment,
    name: Name,
  },
  DeclTypeMismatch {
    env: Environment,
    decl: Declaration,
    given_type: Expr,
  },
  DeclHasMVars {
    env: Environment,
    name: Name,
    expr: Expr,
  },
  FunExpected {
    env: Environment,
    lctx: LocalContext,
    expr: Expr,
  },
  TypeExpected {
    env: Environment,
    lctx: LocalContext,
    expr: Expr,
  },
  LetTypeMismatch {
    env: Environment,
    lctx: LocalContext,
    name: Name,
    given_type: Expr,
    expected_type: Expr,
  },
  ExprTypeMismatch {
    env: Environment,
    lctx: LocalContext,
    expr: Expr,
    expected_type: Expr,
  },
  AppTypeMismatch {
    env: Environment,
    lctx: LocalContext,
    app: Expr,
    fun_type: Expr,
    arg_type: Expr,
  },
  Other {
    msg: String,
  },
}

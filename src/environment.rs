use sp_std::collections::btree_map::BTreeMap;

use alloc::string::String;

use crate::{
  constant::Constant,
  expression::Expr,
  local_context::LocalContext,
  name::Name,
};

pub struct Environment {
  pub constants: BTreeMap<Name, Constant>,
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
    decl: Constant,
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

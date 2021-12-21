use sp_std::collections::btree_map::BTreeMap;

use alloc::string::String;

use crate::{
  constant::Constant,
  expression::Expression,
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
    given_type: Expression,
  },
  DeclHasMVars {
    env: Environment,
    name: Name,
    expr: Expression,
  },
  FunExpected {
    env: Environment,
    lctx: LocalContext,
    expr: Expression,
  },
  TypeExpected {
    env: Environment,
    lctx: LocalContext,
    expr: Expression,
  },
  LetTypeMismatch {
    env: Environment,
    lctx: LocalContext,
    name: Name,
    given_type: Expression,
    expected_type: Expression,
  },
  ExpressionTypeMismatch {
    env: Environment,
    lctx: LocalContext,
    expr: Expression,
    expected_type: Expression,
  },
  AppTypeMismatch {
    env: Environment,
    lctx: LocalContext,
    app: Expression,
    fun_type: Expression,
    arg_type: Expression,
  },
  Other {
    msg: String,
  },
}

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
  expression::Expr,
  local_context::LocalContext,
  name::Name,
};

type ModuleIdx = BigUint;

type CompactedRegion = usize;

pub struct Import {
  pub module: Name,
  pub runtime_only: bool,
}

pub struct EnvironmentHeader {
  pub trust_level: u32,
  pub quot_init: bool,
  pub main_module: Name,
  pub imports: Vec<Import>,
  pub regions: Vec<CompactedRegion>,
  pub module_names: Vec<Name>,
}

pub struct Environment {
  pub const_to_mod_idx: BTreeMap<Name, ModuleIdx>,
  pub constants: BTreeMap<Name, ConstantInfo>,
  pub header: EnvironmentHeader,
}

impl Environment {
  pub fn get(&self, name: &Name) -> Option<&ConstantInfo> {
    self.constants.get(name)
  }

  pub fn add_constant(&mut self, constant: ConstantInfo) {
    self.constants.insert(constant.name(), constant);
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

/// TODO
pub struct EnvExtensionEntry {}

pub struct ModuleData {
  pub imports: Vec<Import>,
  pub constants: Vec<ConstantInfo>,
  pub entries: Vec<(Name, Vec<EnvExtensionEntry>)>,
}

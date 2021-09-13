use sp_std::collections::btree_map::BTreeMap;

use num_bigint::BigUint;
use num_traits::{Zero, One};

use crate::{
  expression::{BinderInfo, Expr},
  name::Name,
};

use sp_im::vector::Vector;

/// Local declarations
#[derive(Clone)]
pub enum LocalDecl {
  CDecl {
    index: BigUint,
    name: Name,
    user_name: Name,
    typ: Expr,
    bi: BinderInfo,
  },
  LDecl {
    index: BigUint,
    name: Name,
    user_name: Name,
    typ: Expr,
    value: Expr,
  },
}

impl LocalDecl {
  pub fn index(&self) -> BigUint {
    match self {
      LocalDecl::CDecl {
        index, ..
      } => index.clone(),
      LocalDecl::LDecl {
        index, ..
      } => index.clone(),
    }
  }
  pub fn name(&self) -> Name {
    match self {
      LocalDecl::CDecl {
        name, ..
      } => name.clone(),
      LocalDecl::LDecl {
        name, ..
      } => name.clone(),
    }
  }
}

pub struct LocalContext {
  fvar_id_to_decl: BTreeMap<Name, LocalDecl>,
  decls: Vector<Option<LocalDecl>>,
  current_index: BigUint,
}

impl Default for LocalContext {
  fn default() -> Self {
    LocalContext {
      fvar_id_to_decl: Default::default(),
      decls: Default::default(),
      current_index: BigUint::zero(),
    }
  }
}

impl LocalContext {

  pub fn add_local_decl_with_value(&self, name: Name, user_name: Name, typ: Expr, value: Expr) {
    let index = &self.current_index + BigUint::one();
    let local_decl = LocalDecl::LDecl {
        index,
        name,
        user_name,
        typ,
        value,
    }
    self.add_local_decl(local_decl);
  }

  pub fn add_local_decl(&self, local_decl: LocalDecl) {
    self.fvar_id_to_decl.insert(local_decl.name(), local_decl);
    self.decls.push_back(Some(local_decl)); 
  }
}

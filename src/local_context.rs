use sp_std::collections::btree_map::BTreeMap;

use num_bigint::BigUint;

use crate::{
  expression::{
    BinderInfo,
    Expr,
  },
  name::Name,
};

use sp_im::vector::Vector;

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

pub struct LocalContext {
  fvar_id_to_decl: BTreeMap<Name, LocalDecl>,
  decls: Vector<Option<LocalDecl>>,
}

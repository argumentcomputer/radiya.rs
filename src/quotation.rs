use crate::{
  declaration::ConstantInfo,
  environment,
  environment::Environment,
  local_context::{
    LocalContext,
    LocalDecl,
  },
  name::{
    Name,
    NameGenerator,
  },
  universe::Univ,
};
use alloc::string::String;
use num_bigint::BigUint;
use num_traits::Zero;
use sp_im::Vector;

/// Quotient types

pub mod quot_consts {
  use crate::name::Name;
  // pub const g_quot: Name = Name::simple(&["Quot"]);
  // pub const g_quot_lift: Name = Name::simple(&["Quot", "lift"]);
  // pub const g_quot_ind: Name = Name::simple(&["Quot", "ind"]);
  // pub const g_quot_mk: Name = Name::simple(&["Quot", "mk"]);
}

pub fn check_eq_type(env: Environment) -> Result<(), String> {
  let eq = Name::simple(&["Eq"]);
  let eq_info = env.get(&eq).ok_or("env does not have Eq constant")?;

  match eq_info {
    ConstantInfo::Inductive(eq_val) => {
      let local_context = LocalContext::default();
      let name_gen = NameGenerator::new(Name::empty());
      let level = Univ::Param(eq);
      let alpha = local_context.add_local_decl_with_value(Name::simple(&["α"]), make_sort(level));
      Ok(())
    }
    _ => Err(format!(
      "failed to initialize quot module,environment does not have an inductive 'Eq' type"
    )),
  }
}

pub fn add_quot(env: &Environment) {}

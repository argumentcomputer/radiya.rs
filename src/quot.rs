use crate::name::Name;
/// Quotient types
use crate::{environment, environment::Environment};
use sp_im::Vector;

pub mod quot_consts {
  use crate::name::Name;
  // pub const g_quot: Name = Name::simple(&["Quot"]);
  // pub const g_quot_lift: Name = Name::simple(&["Quot", "lift"]);
  // pub const g_quot_ind: Name = Name::simple(&["Quot", "ind"]);
  // pub const g_quot_mk: Name = Name::simple(&["Quot", "mk"]);
}

pub fn check_eq_type(env: Environment) -> Result<(), ()> {
  let eq_info = env.get(&Name::simple(&["Eq"]));

  if eq_info.is_inductive() {
    return Err(format!(
      "failed to initialize quot module, environment does not have 'Eq' type"
    ))
  }

  let eq_val = eq_info.to_inductive_val();

  Ok(())
  
}

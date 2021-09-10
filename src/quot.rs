/// Quotient types
use crate::name::{
  Name,
  NameGenerator,
};
use crate::{
  declaration::ConstantInfo, environment, environment::Environment, local_context::LocalContext,
};
use alloc::string::String;
use sp_im::Vector;

pub mod quot_consts {
  use crate::name::Name;
  // pub const g_quot: Name = Name::simple(&["Quot"]);
  // pub const g_quot_lift: Name = Name::simple(&["Quot", "lift"]);
  // pub const g_quot_ind: Name = Name::simple(&["Quot", "ind"]);
  // pub const g_quot_mk: Name = Name::simple(&["Quot", "mk"]);
}

pub fn check_eq_type(env: Environment) -> Result<(), String> {
  let eq_info = env
    .get(&Name::simple(&["Eq"]))
    .ok_or("env does not have Eq constant")?;

  match eq_info {
    ConstantInfo::Inductive(eq_val) => {
      let local_context = LocalContext::default();
      let name_gen = NameGenerator::new(Name::empty());
      Ok(())
    }
    _ => {
      return Err(format!(
        "failed to initialize quot module, environment does not have 'Eq' type"
      ))
    }
  }
}

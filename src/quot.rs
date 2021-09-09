/// Quotient types
use crate::environment;
use crate::name::Name;
use sp_im::Vector;

pub mod quot_consts {
  use crate::name::Name;
  pub const g_quot: Name = Name::simple(&["Quot"]);
  pub const g_quot_lift: Name = Name::simple(&["Quot", "lift"]);
  pub const g_quot_ind: Name = Name::simple(&["Quot", "ind"]);
  pub const g_quot_mk: Name = Name::simple(&["Quot", "mk"]);
}

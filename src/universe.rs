use crate::name::Name;
use sp_std::boxed::Box;

/// Universe levels
#[derive(Clone, Debug)]
pub enum Univ {
  Zero,
  Succ(Box<Univ>),
  Max(Box<Univ>, Box<Univ>),
  IMax(Box<Univ>, Box<Univ>),
  Param(Name),
}

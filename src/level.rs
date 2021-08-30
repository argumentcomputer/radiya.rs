use crate::name::Name;
use sp_std::boxed::Box;

#[derive(Clone, Debug)]
pub enum Level {
  Zero,
  Succ(Box<Level>),
  Max(Box<Level>, Box<Level>),
  IMax(Box<Level>, Box<Level>),
  Param(Name),
}

use crate::name::Name;
use sp_std::boxed::Box;

pub enum Level {
  Zero,
  Succ(Box<Level>),
  Max(Box<Level>, Box<Level>),
  IMax(Box<Level>, Box<Level>),
  Param(Name),
}

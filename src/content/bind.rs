use crate::{
  content::ipld_error::IpldError,
  expression::Bind,
};
use sp_ipld::Ipld;

pub fn bind_to_ipld(bind: &Bind) -> Ipld {
  match bind {
    Bind::Default => Ipld::Integer(0),
    Bind::Implicit => Ipld::Integer(1),
    Bind::Strict => Ipld::Integer(2),
    Bind::Class => Ipld::Integer(3),
  }
}

pub fn bind_from_ipld(ipld: &Ipld) -> Result<Bind, IpldError> {
  match ipld {
    Ipld::Integer(0) => Ok(Bind::Default),
    Ipld::Integer(1) => Ok(Bind::Implicit),
    Ipld::Integer(2) => Ok(Bind::Strict),
    Ipld::Integer(3) => Ok(Bind::Class),
    _ => Err(IpldError::Bind(ipld.clone())),
  }
}

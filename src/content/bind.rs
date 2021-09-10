use crate::{
  content::ipld::IpldError,
  expression::BinderInfo,
};
use sp_ipld::Ipld;

pub fn bind_to_ipld(bind: &BinderInfo) -> Ipld {
  match bind {
    BinderInfo::Default => Ipld::Integer(0),
    BinderInfo::Implicit => Ipld::Integer(1),
    BinderInfo::StrictImplicit => Ipld::Integer(2),
    BinderInfo::InstImplicit => Ipld::Integer(3),
    BinderInfo::Rec => Ipld::Integer(4),
  }
}

pub fn bind_from_ipld(ipld: &Ipld) -> Result<BinderInfo, IpldError> {
  match ipld {
    Ipld::Integer(0) => Ok(BinderInfo::Default),
    Ipld::Integer(1) => Ok(BinderInfo::Implicit),
    Ipld::Integer(2) => Ok(BinderInfo::StrictImplicit),
    Ipld::Integer(3) => Ok(BinderInfo::InstImplicit),
    Ipld::Integer(4) => Ok(BinderInfo::Rec),
    _ => Err(IpldError::ExpectedBinderInfo(ipld.clone())),
  }
}

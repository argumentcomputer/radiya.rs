use sp_std::convert::TryInto;

use crate::{
  declaration::{
    DefinitionSafety,
    ReducibilityHints,
  },
  format::{
    expr::ExprCid,
    ipld_embed::{
      IpldEmbed,
      IpldError,
    },
    name::NameCid,
    univ::UnivCid,
    DECL_CODEC,
  },
};

use alloc::{
  borrow::ToOwned,
  string::String,
};

use num_bigint::BigUint;
use sp_cid::{
  Cid,
  Version,
};
use sp_im::vector::Vector;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
  Ipld,
};
use sp_std::vec::Vec;

use sp_multihash::{
  Code,
  Multihash,
  MultihashDigest,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeclCid {
  // pub indx: u32,
  pub hash: Multihash,
}

impl IpldEmbed for DeclCid {
  fn cid(&self) -> Cid {
    // let codec = DECL_CODEC | (self.indx as u64);
    Cid::new_v1(DECL_CODEC, self.hash)
  }

  fn to_ipld(&self) -> Ipld { Ipld::Link(self.cid()) }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    // let hi = 0xFFFF_FFFF_0000_0000u64;
    // let lo = 0x0000_0000_FFFF_FFFFu64;
    match ipld {
      Ipld::Link(cid)
        //if (cid.codec() & hi == DECL_CODEC) && cid.version() == Version::V1 =>
        if (cid.codec() == DECL_CODEC) && cid.version() == Version::V1 =>
      {
        //Ok(DeclCid { indx: (cid.codec() & lo) as u32, hash: *cid.hash() })
        Ok(DeclCid { hash: *cid.hash() })
      }
      _ => Err(IpldError::ExpectedUnivCid(ipld.clone())),
    }
  }
}

pub fn hints_to_ipld(hints: &ReducibilityHints) -> Ipld {
  match hints {
    ReducibilityHints::Opaque => Ipld::List(vec![Ipld::Integer(0)]),
    ReducibilityHints::Abbrev => Ipld::List(vec![Ipld::Integer(1)]),
    ReducibilityHints::Regular(x) => {
      Ipld::List(vec![Ipld::Integer(2), Ipld::Integer((*x).into())])
    }
  }
}

pub fn hints_from_ipld(ipld: &Ipld) -> Result<ReducibilityHints, IpldError> {
  use Ipld::*;
  match ipld {
    List(xs) => match xs.as_slice() {
      [Ipld::Integer(0)] => Ok(ReducibilityHints::Opaque),
      [Ipld::Integer(1)] => Ok(ReducibilityHints::Abbrev),
      [Ipld::Integer(2), Ipld::Integer(x)] => {
        let x: u32 = (*x).try_into().map_err(IpldError::U32)?;
        Ok(ReducibilityHints::Regular(x))
      }
      _ => Err(IpldError::ExpectedReducibilityHints(ipld.clone())),
    },
    _ => Err(IpldError::ExpectedReducibilityHints(ipld.clone())),
  }
}

pub fn safety_to_ipld(bind: &DefinitionSafety) -> Ipld {
  match bind {
    DefinitionSafety::Safe => Ipld::Integer(0),
    DefinitionSafety::Unsafe => Ipld::Integer(1),
    DefinitionSafety::Partial => Ipld::Integer(2),
  }
}
pub fn safety_from_ipld(ipld: &Ipld) -> Result<DefinitionSafety, IpldError> {
  match ipld {
    Ipld::Integer(0) => Ok(DefinitionSafety::Safe),
    Ipld::Integer(1) => Ok(DefinitionSafety::Unsafe),
    Ipld::Integer(2) => Ok(DefinitionSafety::Partial),
    _ => Err(IpldError::ExpectedDefinitionSafety(ipld.clone())),
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Def {
  pub name: NameCid,
  pub lparams: Vector<NameCid>,
  pub typ: ExprCid,
  pub val: ExprCid,
  pub hints: ReducibilityHints,
  pub safety: DefinitionSafety,
}

impl IpldEmbed for Def {
  fn to_ipld(&self) -> Ipld {
    let mut ls = vec![];
    for lp in &self.lparams {
      ls.push(lp.to_ipld());
    }
    Ipld::List(vec![
      Ipld::String(String::from("#DEF")),
      self.name.to_ipld(),
      Ipld::List(ls),
      self.typ.to_ipld(),
      self.val.to_ipld(),
      hints_to_ipld(&self.hints),
      safety_to_ipld(&self.safety),
    ])
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      DECL_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), n, List(ls), t, v, h, s] if tag == "#DEF" => {
          let name = NameCid::from_ipld(n)?;
          let mut levels = Vec::new();
          for l in ls {
            let level = NameCid::from_ipld(l)?;
            levels.push(level);
          }
          let typ = ExprCid::from_ipld(t)?;
          let val = ExprCid::from_ipld(v)?;
          let hints = hints_from_ipld(h)?;
          let safety = safety_from_ipld(s)?;
          Ok(Def { name, lparams: levels.into(), typ, val, hints, safety })
        }
        xs => Err(IpldError::ExpectedDef(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedDef(xs.to_owned())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Axiom {
  pub name: NameCid,
  pub lparams: Vector<NameCid>,
  pub typ: ExprCid,
  pub is_unsafe: bool,
}

impl IpldEmbed for Axiom {
  fn to_ipld(&self) -> Ipld {
    let mut ls = vec![];
    for lp in &self.lparams {
      ls.push(lp.to_ipld());
    }
    Ipld::List(vec![
      Ipld::String(String::from("#AX")),
      self.name.to_ipld(),
      Ipld::List(ls),
      self.typ.to_ipld(),
      Ipld::Bool(self.is_unsafe),
    ])
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      DECL_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), n, List(ls), t, Bool(u)] if tag == "#AX" => {
          let name = NameCid::from_ipld(n)?;
          let mut levels = Vec::new();
          for l in ls {
            let level = NameCid::from_ipld(l)?;
            levels.push(level);
          }
          let typ = ExprCid::from_ipld(t)?;
          Ok(Axiom { name, lparams: levels.into(), typ, is_unsafe: *u })
        }
        xs => Err(IpldError::ExpectedAxiom(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedAxiom(xs.to_owned())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Theorem {
  pub name: NameCid,
  pub lparams: Vector<NameCid>,
  pub typ: ExprCid,
  pub val: ExprCid,
}

impl IpldEmbed for Theorem {
  fn to_ipld(&self) -> Ipld {
    let mut ls = vec![];
    for lp in &self.lparams {
      ls.push(lp.to_ipld());
    }
    Ipld::List(vec![
      Ipld::String(String::from("#THM")),
      self.name.to_ipld(),
      Ipld::List(ls),
      self.typ.to_ipld(),
      self.val.to_ipld(),
    ])
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      DECL_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), n, List(ls), t, v] if tag == "#THM" => {
          let name = NameCid::from_ipld(n)?;
          let mut levels = Vec::new();
          for l in ls {
            let level = NameCid::from_ipld(l)?;
            levels.push(level);
          }
          let typ = ExprCid::from_ipld(t)?;
          let val = ExprCid::from_ipld(v)?;
          Ok(Theorem { name, lparams: levels.into(), typ, val })
        }
        xs => Err(IpldError::ExpectedTheorem(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedTheorem(xs.to_owned())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Opaque {
  pub name: NameCid,
  pub lparams: Vector<NameCid>,
  pub typ: ExprCid,
  pub val: ExprCid,
  pub is_unsafe: bool,
}

impl IpldEmbed for Opaque {
  fn to_ipld(&self) -> Ipld {
    let mut ls = vec![];
    for lp in &self.lparams {
      ls.push(lp.to_ipld());
    }
    Ipld::List(vec![
      Ipld::String(String::from("#OPQ")),
      self.name.to_ipld(),
      Ipld::List(ls),
      self.typ.to_ipld(),
      self.val.to_ipld(),
      Ipld::Bool(self.is_unsafe),
    ])
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      DECL_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), n, List(ls), t, v, Bool(u)] if tag == "#OPQ" => {
          let name = NameCid::from_ipld(n)?;
          let mut levels = Vec::new();
          for l in ls {
            let level = NameCid::from_ipld(l)?;
            levels.push(level);
          }
          let typ = ExprCid::from_ipld(t)?;
          let val = ExprCid::from_ipld(v)?;
          Ok(Opaque { name, lparams: levels.into(), typ, val, is_unsafe: *u })
        }
        xs => Err(IpldError::ExpectedAxiom(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedAxiom(xs.to_owned())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Constructor {
  pub name: NameCid,
  pub typ: ExprCid,
}

impl Constructor {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![self.name.to_ipld(), self.typ.to_ipld()])
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Constructor, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [n, t] => {
          let name = NameCid::from_ipld(n)?;
          let typ = ExprCid::from_ipld(t)?;
          Ok(Constructor { name, typ })
        }
        _ => Err(IpldError::ExpectedConstructor(ipld.clone())),
      },
      _ => Err(IpldError::ExpectedConstructor(ipld.clone())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InductiveType {
  pub name: NameCid,
  pub typ: ExprCid,
  pub ctors: Vector<Constructor>,
}

impl InductiveType {
  pub fn to_ipld(&self) -> Ipld {
    let mut cs = vec![];
    for c in &self.ctors {
      cs.push(c.to_ipld())
    }
    Ipld::List(vec![self.name.to_ipld(), self.typ.to_ipld(), Ipld::List(cs)])
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [n, t, List(cs)] => {
          let name = NameCid::from_ipld(n)?;
          let typ = ExprCid::from_ipld(t)?;
          let mut ctors = vec![];
          for c in cs {
            let c = Constructor::from_ipld(c)?;
            ctors.push(c);
          }
          Ok(InductiveType { name, typ, ctors: ctors.into() })
        }
        _ => Err(IpldError::ExpectedConstructor(ipld.clone())),
      },
      _ => Err(IpldError::ExpectedConstructor(ipld.clone())),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Decl {
  Axiom(Axiom),
  Theorem(Theorem),
  Opaque(Opaque),
  Def(Def),
  Quot,
  Mutual(Vector<Def>),
  Inductive {
    lparams: Vector<NameCid>,
    nparams: u64,
    types: Vector<InductiveType>,
    is_unsafe: bool,
  },
}

impl IpldEmbed for Decl {
  fn to_ipld(&self) -> Ipld {
    match self {
      Self::Axiom(x) => x.to_ipld(),
      Self::Theorem(x) => x.to_ipld(),
      Self::Opaque(x) => x.to_ipld(),
      Self::Def(x) => x.to_ipld(),
      Self::Quot => Ipld::List(vec![Ipld::String(String::from("#QUOT"))]),
      Self::Mutual(defs) => {
        let mut ds = vec![];
        for d in defs {
          ds.push(d.to_ipld());
        }
        Ipld::List(vec![Ipld::String(String::from("#MUT")), Ipld::List(ds)])
      }
      Self::Inductive { lparams, nparams, types, is_unsafe } => {
        let mut ls = vec![];
        let mut ts = vec![];
        for lp in lparams {
          ls.push(lp.to_ipld());
        }
        for t in types {
          ts.push(t.to_ipld());
        }
        Ipld::List(vec![
          Ipld::String(String::from("#IND")),
          Ipld::List(ls),
          Ipld::Integer((*nparams).into()),
          Ipld::List(ts),
          Ipld::Bool(*is_unsafe),
        ])
      }
    }
  }

  fn cid(&self) -> Cid {
    Cid::new_v1(
      DECL_CODEC,
      Code::Blake3_256.digest(
        DagCborCodec.encode(&self.to_ipld()).unwrap().into_inner().as_ref(),
      ),
    )
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::*;
    match ipld {
      List(xs) => match xs.as_slice() {
        [String(tag), n, List(ls), t, Bool(u)] if tag == "#AX" => {
          Axiom::from_ipld(ipld).map(Decl::Axiom)
        }
        [String(tag), n, List(ls), t, v] if tag == "#THM" => {
          Theorem::from_ipld(ipld).map(Decl::Theorem)
        }
        [String(tag), n, List(ls), t, v, Bool(u)] if tag == "#OPQ" => {
          Opaque::from_ipld(ipld).map(Decl::Opaque)
        }
        [String(tag), n, List(ls), t, v, h, s] if tag == "#DEF" => {
          Def::from_ipld(ipld).map(Decl::Def)
        }
        [String(tag)] if tag == "#QUOT" => Ok(Decl::Quot),
        [String(tag), List(ls)] if tag == "#MUT" => todo!(),
        [String(tag), List(ls), Integer(n), List(ts), Bool(u)]
          if tag == "#IND" =>
        {
          todo!()
        }
        xs => Err(IpldError::ExpectedDecl(List(xs.to_owned()))),
      },
      xs => Err(IpldError::ExpectedDecl(xs.to_owned())),
    }
  }
}
#[cfg(test)]
pub mod tests {
  use crate::{
    format::tests::arbitrary_cid,
    tests::{
      arbitrary_big_uint,
      frequency,
      gen_range,
    },
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;

  impl Arbitrary for DeclCid {
    fn arbitrary(g: &mut Gen) -> Self {
      let indx: u32 = Arbitrary::arbitrary(g);
      let cid = arbitrary_cid(g, DECL_CODEC & (indx as u64));
      DeclCid { indx, hash: *cid.hash() }
    }
  }

  #[quickcheck]
  fn decl_cid_ipld(x: DeclCid) -> bool {
    match DeclCid::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

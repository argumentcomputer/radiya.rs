use crate::{
  environment::{
    ConstAnonCid,
    ConstCid,
    ConstMetaCid,
    Env,
    EnvError,
    ExprAnonCid,
    ExprMetaCid,
  },
  expression::Expr,
  name::Name,
  nat::Nat,
};

use serde::{
  Deserialize,
  Serialize,
};

use libipld::serde::to_ipld;

use multihash::{
  Code,
  MultihashDigest,
};

use libipld::{
  cbor::DagCborCodec,
  codec::Codec,
};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum DefSafety {
  Unsafe,
  Safe,
  Partial,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum QuotKind {
  Type,
  Ctor,
  Lift,
  Ind,
}

#[derive(Clone, Debug)]
pub enum Const {
  Axiom {
    lvl: Vec<Name>,
    typ: Box<Expr>,
  },
  Theorem {
    lvl: Vec<Name>,
    typ: Box<Expr>,
    expr: Box<Expr>,
  },
  Opaque {
    lvl: Vec<Name>,
    typ: Box<Expr>,
    expr: Box<Expr>,
    safe: bool,
  },
  Definition {
    lvl: Vec<Name>,
    typ: Box<Expr>,
    expr: Box<Expr>,
    safe: DefSafety,
  },
  Inductive {
    lvl: Vec<Name>,
    typ: Box<Expr>,
    params: Nat,
    indices: Nat,
    unit: bool,
    rec: bool,
    safe: bool,
    refl: bool,
    nested: bool,
  },
  Constructor {
    lvl: Vec<Name>,
    ind: ConstCid,
    typ: Box<Expr>,
    idx: Nat,
    param: Nat,
    field: Nat,
    safe: bool,
  },
  Recursor {
    lvl: Vec<Name>,
    ind: ConstCid,
    typ: Box<Expr>,
    params: Nat,
    indices: Nat,
    motives: Nat,
    minors: Nat,
    rules: Vec<(ConstCid, Nat, Expr)>,
    k: bool,
    safe: bool,
  },
  Quotient {
    kind: QuotKind,
  },
}

impl Const {
  pub fn cid(&self, env: &mut Env) -> Result<ConstCid, EnvError> {
    match self {
      Const::Axiom { lvl, typ } => {
        let typ_cid = typ.clone().store(env)?;
        let anon =
          ConstAnon::Axiom(lvl.len().into(), typ_cid.anon).store(env)?;
        let meta = ConstMeta::Axiom(lvl.clone(), typ_cid.meta).store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Theorem { lvl, typ, expr } => {
        let typ_cid = typ.clone().store(env)?;
        let expr_cid = expr.clone().store(env)?;
        let anon =
          ConstAnon::Theorem(lvl.len().into(), typ_cid.anon, expr_cid.anon)
            .store(env)?;
        let meta = ConstMeta::Theorem(lvl.clone(), typ_cid.meta, expr_cid.meta)
          .store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Opaque { lvl, typ, expr, safe } => {
        let typ_cid = typ.clone().store(env)?;
        let expr_cid = expr.clone().store(env)?;
        let anon = ConstAnon::Opaque(
          lvl.len().into(),
          typ_cid.anon,
          expr_cid.anon,
          *safe,
        )
        .store(env)?;
        let meta = ConstMeta::Opaque(lvl.clone(), typ_cid.meta, expr_cid.meta)
          .store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Definition { lvl, typ, expr, safe } => {
        let typ_cid = typ.clone().store(env)?;
        let expr_cid = expr.clone().store(env)?;
        let anon = ConstAnon::Definition(
          lvl.len().into(),
          typ_cid.anon,
          expr_cid.anon,
          *safe,
        )
        .store(env)?;
        let meta =
          ConstMeta::Definition(lvl.clone(), typ_cid.meta, expr_cid.meta)
            .store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Inductive {
        lvl,
        typ,
        params,
        indices,
        unit,
        rec,
        safe,
        refl,
        nested,
      } => {
        let typ_cid = typ.clone().store(env)?;
        let anon = ConstAnon::Inductive(
          lvl.len().into(),
          typ_cid.anon,
          params.clone(),
          indices.clone(),
          *unit,
          *rec,
          *safe,
          *refl,
          *nested,
        )
        .store(env)?;
        let meta =
          ConstMeta::Inductive(lvl.clone(), typ_cid.meta).store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Constructor { lvl, ind, typ, idx, param, field, safe } => {
        let typ_cid = typ.clone().store(env)?;
        let anon = ConstAnon::Constructor(
          lvl.len().into(),
          ind.anon,
          typ_cid.anon,
          idx.clone(),
          param.clone(),
          field.clone(),
          *safe,
        )
        .store(env)?;
        let meta = ConstMeta::Constructor(lvl.clone(), ind.meta, typ_cid.meta)
          .store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Recursor {
        lvl,
        ind,
        typ,
        params,
        indices,
        motives,
        minors,
        rules,
        k,
        safe,
      } => {
        let typ_cid = typ.clone().store(env)?;
        let mut rules_anon: Vec<(ConstAnonCid, Nat, ExprAnonCid)> = Vec::new();
        let mut rules_meta: Vec<(ConstMetaCid, ExprMetaCid)> = Vec::new();
        for (ctor_cid, fields, rhs) in rules {
          let expr_cid = rhs.clone().store(env)?;
          rules_anon.push((ctor_cid.anon, fields.clone(), expr_cid.anon));
          rules_meta.push((ctor_cid.meta, expr_cid.meta));
        }
        let anon = ConstAnon::Recursor(
          lvl.len().into(),
          ind.anon,
          typ_cid.anon,
          params.clone(),
          indices.clone(),
          motives.clone(),
          minors.clone(),
          rules_anon,
          *k,
          *safe,
        )
        .store(env)?;
        let meta =
          ConstMeta::Recursor(lvl.clone(), ind.meta, typ_cid.meta, rules_meta)
            .store(env)?;
        Ok(ConstCid { anon, meta })
      }
      Const::Quotient { kind } => {
        let anon = ConstAnon::Quotient(*kind).store(env)?;
        let meta = ConstMeta::Quotient.store(env)?;
        Ok(ConstCid { anon, meta })
      }
    }
  }

  pub fn store(self, env: &mut Env) -> Result<ConstCid, EnvError> {
    let cid = self.cid(env)?;
    env.insert_const(cid, self)?;
    Ok(cid)
  }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ConstMeta {
  Axiom(Vec<Name>, ExprMetaCid),
  Theorem(Vec<Name>, ExprMetaCid, ExprMetaCid),
  Opaque(Vec<Name>, ExprMetaCid, ExprMetaCid),
  Definition(Vec<Name>, ExprMetaCid, ExprMetaCid),
  Inductive(Vec<Name>, ExprMetaCid),
  Constructor(Vec<Name>, ConstMetaCid, ExprMetaCid),
  Recursor(
    Vec<Name>,
    ConstMetaCid,
    ExprMetaCid,
    Vec<(ConstMetaCid, ExprMetaCid)>,
  ),
  Quotient,
}

impl ConstMeta {
  pub fn cid(&self) -> Result<ConstMetaCid, EnvError> {
    let ipld = to_ipld(self).map_err(|e| EnvError::IpldError(e))?;
    let bytes =
      DagCborCodec.encode(&ipld).map_err(|e| EnvError::CborError(e))?;
    Ok(ConstMetaCid::new(Code::Sha3_256.digest(&bytes)))
  }

  pub fn store(self, env: &mut Env) -> Result<ConstMetaCid, EnvError> {
    let cid = self.cid()?;
    env.insert_const_meta(cid, self)?;
    Ok(cid)
  }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ConstAnon {
  Axiom(Nat, ExprAnonCid),
  Theorem(Nat, ExprAnonCid, ExprAnonCid),
  Opaque(Nat, ExprAnonCid, ExprAnonCid, bool),
  Definition(Nat, ExprAnonCid, ExprAnonCid, DefSafety),
  Inductive(Nat, ExprAnonCid, Nat, Nat, bool, bool, bool, bool, bool),
  Constructor(Nat, ConstAnonCid, ExprAnonCid, Nat, Nat, Nat, bool),
  Recursor(
    Nat,
    ConstAnonCid,
    ExprAnonCid,
    Nat,
    Nat,
    Nat,
    Nat,
    Vec<(ConstAnonCid, Nat, ExprAnonCid)>,
    bool,
    bool,
  ),
  Quotient(QuotKind),
}

impl ConstAnon {
  pub fn cid(&self) -> Result<ConstAnonCid, EnvError> {
    let ipld = to_ipld(self).map_err(|e| EnvError::IpldError(e))?;
    let bytes =
      DagCborCodec.encode(&ipld).map_err(|e| EnvError::CborError(e))?;
    Ok(ConstAnonCid::new(Code::Sha3_256.digest(&bytes)))
  }

  pub fn store(self, env: &mut Env) -> Result<ConstAnonCid, EnvError> {
    let cid = self.cid()?;
    env.insert_const_anon(cid, self)?;
    Ok(cid)
  }
}

use crate::{
  constant::Constant,
  content::{
    cache::Cache,
    cid::{
      ConstCid,
      ExprCid,
      LitCid,
      NameCid,
      UnivCid,
      UnivMetaCid,
    },
    constant::Const,
    environment::Environment,
    expr::{
      Expr,
      ExprMeta,
    },
    name::Name,
    univ::{
      Univ,
      UnivMeta,
    },
  },
  expression::{
    BinderInfo,
    Expression,
    Literal,
  },
  name,
  name::NamePart,
  parse::position::Pos,
  universe::Universe,
};

use num_bigint::BigUint;
use sp_std::{
  boxed::Box,
  convert::TryInto,
  vec::Vec,
};

#[derive(PartialEq, Debug, Clone)]
pub enum EmbedError {
  Univ(Univ, UnivMeta),
  Expr(Expr, ExprMeta),
  IdxOverflow,
  CacheGetLit(LitCid),
  CacheGetName(NameCid),
  CacheGetUniv(UnivCid),
  CacheGetUnivMeta(UnivMetaCid),
  CacheGetExpr(ExprCid),
  CacheGetConst(ConstCid),
  UndefinedConst(name::Name, NameCid),
  FreeVar(Pos, usize, name::Name, BinderInfo),
}

impl name::Name {
  pub fn embed(&self, cache: &mut Cache) -> Result<NameCid, EmbedError> {
    let mut prev = cache.put_name(Name::Anon)?;
    for p in self.parts.iter() {
      match p {
        NamePart::Str(s) => {
          prev = cache.put_name(Name::Str { prev, name: s.clone() })?
        }
        NamePart::Int(x) => {
          prev = cache.put_name(Name::Int { prev, int: x.clone() })?
        }
      }
    }
    Ok(prev)
  }

  pub fn unembed(
    cache: &mut Cache,
    cid: NameCid,
  ) -> Result<name::Name, EmbedError> {
    let mut parts = Vec::new();
    let mut cid = cid;
    loop {
      let name = cache.get_name(&cid)?;
      match name {
        Name::Anon => break,
        Name::Str { prev, name } => {
          parts.push(NamePart::Str(name.clone()));
          cid = *prev;
        }
        Name::Int { prev, int } => {
          parts.push(NamePart::Int(int.clone()));
          cid = *prev;
        }
      }
    }
    Ok(name::Name { parts: parts.into_iter().rev().collect() })
  }
}

impl Universe {
  pub fn embed(
    &self,
    cache: &mut Cache,
  ) -> Result<(UnivCid, UnivMetaCid), EmbedError> {
    match self {
      Universe::Zero => {
        let univ = cache.put_univ(Univ::Zero)?;
        let meta = cache.put_univ_meta(UnivMeta::Zero)?;
        Ok((univ, meta))
      }
      Universe::Succ(x) => {
        let (pred, pred_meta) = x.embed(cache)?;
        let univ = cache.put_univ(Univ::Succ { pred })?;
        let meta = cache.put_univ_meta(UnivMeta::Succ(pred_meta))?;
        Ok((univ, meta))
      }
      Universe::Max(lhs, rhs) => {
        let (lhs, lhs_meta) = lhs.embed(cache)?;
        let (rhs, rhs_meta) = rhs.embed(cache)?;
        let univ = cache.put_univ(Univ::Max { lhs, rhs })?;
        let meta = cache.put_univ_meta(UnivMeta::Max(lhs_meta, rhs_meta))?;
        Ok((univ, meta))
      }
      Universe::IMax(lhs, rhs) => {
        let (lhs, lhs_meta) = lhs.embed(cache)?;
        let (rhs, rhs_meta) = rhs.embed(cache)?;
        let univ = cache.put_univ(Univ::IMax { lhs, rhs })?;
        let meta = cache.put_univ_meta(UnivMeta::IMax(lhs_meta, rhs_meta))?;
        Ok((univ, meta))
      }
      Universe::Param(name, idx) => {
        let name = name.clone().embed(cache)?;
        let univ = cache.put_univ(Univ::Param { idx: (*idx).into() })?;
        let meta = cache.put_univ_meta(UnivMeta::Param(name))?;
        Ok((univ, meta))
      }
    }
  }

  pub fn unembed(
    cache: &mut Cache,
    univ: UnivCid,
    meta: UnivMetaCid,
  ) -> Result<Self, EmbedError> {
    let univ = cache.get_univ(&univ)?.clone();
    let meta = cache.get_univ_meta(&meta)?.clone();
    match (univ, meta) {
      (Univ::Zero, UnivMeta::Zero) => Ok(Universe::Zero),
      (Univ::Succ { pred }, UnivMeta::Succ(pred_meta)) => {
        let pred = Universe::unembed(cache, pred, pred_meta)?;
        Ok(Universe::Succ(Box::new(pred)))
      }
      (Univ::Max { lhs, rhs }, UnivMeta::Max(lhs_meta, rhs_meta)) => {
        let lhs = Universe::unembed(cache, lhs, lhs_meta)?;
        let rhs = Universe::unembed(cache, rhs, rhs_meta)?;
        Ok(Universe::Max(Box::new(lhs), Box::new(rhs)))
      }
      (Univ::IMax { lhs, rhs }, UnivMeta::IMax(lhs_meta, rhs_meta)) => {
        let lhs = Universe::unembed(cache, lhs, lhs_meta)?;
        let rhs = Universe::unembed(cache, rhs, rhs_meta)?;
        Ok(Universe::IMax(Box::new(lhs), Box::new(rhs)))
      }
      (Univ::Param { idx }, UnivMeta::Param(name)) => {
        let name = name::Name::unembed(cache, name)?;
        let idx: usize = idx.try_into().map_err(|_| EmbedError::IdxOverflow)?;
        Ok(Universe::Param(name, idx))
      }
      (univ, meta) => Err(EmbedError::Univ(univ, meta)),
    }
  }
}

// impl Literal {
//  pub fn embed(&self, cache: &mut Cache) -> Result<LiteralCid, EmbedError> {
//    cache.put_literal(self.clone())
//  }
//}
// impl Expression {
//  pub fn embed(
//    &self,
//    cache: &mut Cache,
//    env: &Environment,
//  ) -> Result<(MetaCid, ExprCid), EmbedError> {
//    match self {
//      Self::BVar(pos, idx) => {
//        let meta = cache.put_meta(Metadata::leaf(*pos))?;
//        let expr = cache.put_expr(Expr::Var { idx: (*idx).into() })?;
//        Ok((meta, expr))
//      }
//      Self::FVar(pos, idx, name, bind, _) => {
//        Err(EmbedError::FreeVar(*pos, *idx, name.clone(), *bind))
//      }
//      Self::Sort(pos, univ) => {
//        let (univ_meta, univ) = (*univ).embed(cache)?;
//        let meta = cache.put_meta(Metadata::branch(*pos, vec![univ_meta]))?;
//        let expr = cache.put_expr(Expr::Sort { univ })?;
//        Ok((meta, expr))
//      }
//      Self::Const(pos, name, levels) => {
//        let name_cid = name.clone().embed(cache)?;
//        let (const_meta_cid, const_cid) = env
//          .constants
//          .get(&name_cid)
//          .ok_or(EmbedError::UndefinedConst(name.clone(), name_cid))?;
//        let mut levels_meta = Vec::new();
//        let mut levels_univ = Vec::new();
//        for l in levels {
//          let (l_meta, l_univ) = l.embed(cache)?;
//          levels_meta.push(l_meta);
//          levels_univ.push(l_univ);
//        }
//        let meta = cache.put_meta(Metadata::refer(
//          *pos,
//          name_cid,
//          *const_meta_cid,
//          levels_meta,
//        ))?;
//        let expr = cache.put_expr(Expr::Const {
//          constant: *const_cid,
//          levels: levels_univ.into(),
//        })?;
//        Ok((meta, expr))
//      }
//      Self::App(pos, fun, arg) => {
//        let (fun_meta, fun) = fun.embed(cache, env)?;
//        let (arg_meta, arg) = arg.embed(cache, env)?;
//        let meta =
//          cache.put_meta(Metadata::branch(*pos, vec![fun_meta, arg_meta]))?;
//        let expr = cache.put_expr(Expr::App { fun, arg })?;
//        Ok((meta, expr))
//      }
//      Self::Lam(pos, name, info, typ, bod) => {
//        let name = name.embed(cache)?;
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let (bod_meta, bod) = bod.embed(cache, env)?;
//        let meta = cache
//          .put_meta(Metadata::name(*pos, name, vec![typ_meta, bod_meta]))?;
//        let expr = cache.put_expr(Expr::Lam { info: *info, typ, bod })?;
//        Ok((meta, expr))
//      }
//      Self::Pi(pos, name, info, typ, bod) => {
//        let name = name.embed(cache)?;
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let (bod_meta, bod) = bod.embed(cache, env)?;
//        let meta = cache
//          .put_meta(Metadata::name(*pos, name, vec![typ_meta, bod_meta]))?;
//        let expr = cache.put_expr(Expr::Pi { info: *info, typ, bod })?;
//        Ok((meta, expr))
//      }
//      Self::Let(pos, name, typ, val, bod) => {
//        let name = name.embed(cache)?;
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let (val_meta, val) = val.embed(cache, env)?;
//        let (bod_meta, bod) = bod.embed(cache, env)?;
//        let meta = cache.put_meta(Metadata::name(*pos, name, vec![
//          typ_meta, val_meta, bod_meta,
//        ]))?;
//        let expr = cache.put_expr(Expr::Let { typ, val, bod })?;
//        Ok((meta, expr))
//      }
//      Self::Lit(pos, val) => {
//        let meta = cache.put_meta(Metadata::leaf(*pos))?;
//        let val = val.embed(cache)?;
//        let expr = cache.put_expr(Expr::Lit { val })?;
//        Ok((meta, expr))
//      }
//      Self::Fix(pos, bod) => {
//        let (bod_meta, bod) = bod.embed(cache, env)?;
//        let meta = cache.put_meta(Metadata::branch(*pos, vec![bod_meta]))?;
//        let expr = cache.put_expr(Expr::Fix { bod })?;
//        Ok((meta, expr))
//      }
//    }
//  }
//}
// impl Constant {
//  pub fn embed(
//    &self,
//    cache: &mut Cache,
//    env: &Environment,
//  ) -> Result<(MetaCid, ConstCid), EmbedError> {
//    match self {
//      Self::Quotient { pos, name, level_params, typ, kind } => {
//        let name = name.embed(cache)?;
//        let mut ls = Vec::new();
//        let mut n: usize = 0;
//        for l in level_params {
//          let l = l.embed(cache)?;
//          ls.push(l);
//          n = n + 1;
//        }
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let meta =
//          cache.put_meta(Metadata::constant(*pos, name, ls, vec![typ_meta]))?;
//        let cons = cache.put_constant(Const::Quotient {
//          levels: n.into(),
//          typ,
//          kind: *kind,
//        })?;
//        Ok((meta, cons))
//      }
//      Self::Axiom { pos, name, level_params, typ, is_unsafe } => {
//        let name = name.embed(cache)?;
//        let mut ls = Vec::new();
//        let mut n: usize = 0;
//        for l in level_params {
//          let l = l.embed(cache)?;
//          ls.push(l);
//          n = n + 1;
//        }
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let meta =
//          cache.put_meta(Metadata::constant(*pos, name, ls, vec![typ_meta]))?;
//        let cons = cache.put_constant(Const::Axiom {
//          levels: n.into(),
//          typ,
//          is_unsafe: *is_unsafe,
//        })?;
//        Ok((meta, cons))
//      }
//      Self::Theorem { pos, name, level_params, typ, val } => {
//        let name = name.embed(cache)?;
//        let mut ls = Vec::new();
//        let mut n: usize = 0;
//        for l in level_params {
//          let l = l.embed(cache)?;
//          ls.push(l);
//          n = n + 1;
//        }
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let (val_meta, val) = val.embed(cache, env)?;
//        let meta = cache.put_meta(Metadata::constant(*pos, name, ls, vec![
//          typ_meta, val_meta,
//        ]))?;
//        let cons =
//          cache.put_constant(Const::Theorem { levels: n.into(), typ, val })?;
//        Ok((meta, cons))
//      }
//      Self::Opaque { pos, name, level_params, typ, val, is_unsafe } => {
//        let name = name.embed(cache)?;
//        let mut ls = Vec::new();
//        let mut n: usize = 0;
//        for l in level_params {
//          let l = l.embed(cache)?;
//          ls.push(l);
//          n = n + 1;
//        }
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let (val_meta, val) = val.embed(cache, env)?;
//        let meta = cache.put_meta(Metadata::constant(*pos, name, ls, vec![
//          typ_meta, val_meta,
//        ]))?;
//        let cons = cache.put_constant(Const::Opaque {
//          levels: n.into(),
//          typ,
//          val,
//          is_unsafe: *is_unsafe,
//        })?;
//        Ok((meta, cons))
//      }
//      Self::Definition { pos, name, level_params, typ, val, safety } => {
//        let name = name.embed(cache)?;
//        let mut ls = Vec::new();
//        let mut n: usize = 0;
//        for l in level_params {
//          let l = l.embed(cache)?;
//          ls.push(l);
//          n = n + 1;
//        }
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        let (val_meta, val) = val.embed(cache, env)?;
//        let meta = cache.put_meta(Metadata::constant(*pos, name, ls, vec![
//          typ_meta, val_meta,
//        ]))?;
//        let cons = cache.put_constant(Const::Definition {
//          levels: n.into(),
//          typ,
//          val,
//          safety: *safety,
//        })?;
//        Ok((meta, cons))
//      }
//      Self::Inductive {
//        pos,
//        name,
//        level_params,
//        typ,
//        ctors,
//        params,
//        indices,
//        is_unsafe,
//      } => {
//        let name = name.embed(cache)?;
//        let mut ls = Vec::new();
//        let mut n: usize = 0;
//        for l in level_params {
//          let l = l.embed(cache)?;
//          ls.push(l);
//          n = n + 1;
//        }
//        let (typ_meta, typ) = typ.embed(cache, env)?;
//        todo!()
//      }
//      _ => todo!(),
//    }
//  }
//}
//
#[cfg(test)]
pub mod tests {

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  #[quickcheck]
  fn name_embed(name: name::Name) -> bool { name_embed_inner(name) == Ok(true) }

  fn name_embed_inner(name: name::Name) -> Result<bool, EmbedError> {
    let mut cache = Cache::new();
    let cid = name.embed(&mut cache)?;
    let name2 = name::Name::unembed(&mut cache, cid)?;
    Ok(name == name2)
  }
  #[quickcheck]
  fn univ_embed(univ: Universe) -> bool {
    println!("{:?}", univ);
    match univ_embed_inner(univ) {
      Ok(x) => x,
      Err(x) => {
        println!("{:?}", x);
        false
      }
    }
  }

  fn univ_embed_inner(univ: Universe) -> Result<bool, EmbedError> {
    let mut cache = Cache::new();
    let (cid, meta_cid) = univ.embed(&mut cache)?;
    let univ2 = Universe::unembed(&mut cache, cid, meta_cid)?;
    Ok(univ == univ2)
  }
}

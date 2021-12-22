use crate::{
  constant::Constant,
  content::{
    cache::Cache,
    cid::{
      ConstCid,
      ConstMetaCid,
      ExprCid,
      ExprMetaCid,
      LitCid,
      NameCid,
      UnivCid,
      UnivMetaCid,
    },
    constant::{
      Const,
      ConstMeta,
    },
    environment::Env,
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

use sp_im::vector::Vector;
use sp_std::{
  boxed::Box,
  convert::TryInto,
  vec::Vec,
};

#[derive(PartialEq, Debug, Clone)]
pub enum EmbedError {
  Univ(Univ, UnivMeta),
  Expr(Expr, ExprMeta),
  Const(Const, ConstMeta),
  BigUintOverflow,
  LevelLengthMismatch,
  CacheGetLit(LitCid),
  CacheGetName(NameCid),
  CacheGetUniv(UnivCid),
  CacheGetUnivMeta(UnivMetaCid),
  CacheGetExpr(ExprCid),
  CacheGetExprMeta(ExprMetaCid),
  CacheGetConst(ConstCid),
  CacheGetConstMeta(ConstMetaCid),
  UnexpectedInductive(Constant, ConstCid, ConstMetaCid),
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
  // TODO: refactor to iterative version to avoid stack overflows
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

  // TODO: refactor to iterative version to avoid stack overflows
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
        let idx: usize =
          idx.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        Ok(Universe::Param(name, idx))
      }
      (univ, meta) => Err(EmbedError::Univ(univ, meta)),
    }
  }
}

impl Literal {
  pub fn embed(&self, cache: &mut Cache) -> Result<LitCid, EmbedError> {
    cache.put_literal(self.clone())
  }

  pub fn unembed(
    cache: &mut Cache,
    cid: LitCid,
  ) -> Result<Literal, EmbedError> {
    cache.get_literal(&cid).map(|x| x.clone())
  }
}

impl Expression {
  // TODO: refactor to iterative version to avoid stack overflows
  pub fn embed(
    &self,
    cache: &mut Cache,
    env: &Env,
  ) -> Result<(ExprCid, ExprMetaCid), EmbedError> {
    match self {
      Self::BVar(pos, idx) => {
        let expr = cache.put_expr(Expr::Var { idx: (*idx).into() })?;
        let meta = cache.put_expr_meta(ExprMeta::Var(*pos))?;
        Ok((expr, meta))
      }
      Self::FVar(pos, idx, name, bind, _) => {
        Err(EmbedError::FreeVar(*pos, *idx, name.clone(), *bind))
      }
      Self::Sort(pos, univ) => {
        let (univ, univ_meta) = (*univ).embed(cache)?;
        let expr = cache.put_expr(Expr::Sort { univ })?;
        let meta = cache.put_expr_meta(ExprMeta::Sort(*pos, univ_meta))?;
        Ok((expr, meta))
      }
      Self::Const(pos, name, levels) => {
        let name_cid = name.clone().embed(cache)?;
        let (const_cid, const_meta_cid) = env
          .constants
          .get(&name_cid)
          .ok_or(EmbedError::UndefinedConst(name.clone(), name_cid))?;
        let mut levels_univ = Vec::new();
        let mut levels_meta = Vec::new();
        for l in levels {
          let (l_univ, l_meta) = l.embed(cache)?;
          levels_univ.push(l_univ);
          levels_meta.push(l_meta);
        }
        let meta = cache.put_expr_meta(ExprMeta::Const(
          *pos,
          name_cid,
          *const_meta_cid,
          levels_meta,
        ))?;
        let expr = cache.put_expr(Expr::Const {
          constant: *const_cid,
          levels: levels_univ.into(),
        })?;
        Ok((expr, meta))
      }
      Self::App(pos, fun, arg) => {
        let (fun, fun_meta) = fun.embed(cache, env)?;
        let (arg, arg_meta) = arg.embed(cache, env)?;
        let expr = cache.put_expr(Expr::App { fun, arg })?;
        let meta =
          cache.put_expr_meta(ExprMeta::App(*pos, fun_meta, arg_meta))?;
        Ok((expr, meta))
      }
      Self::Lam(pos, name, info, typ, bod) => {
        let name = name.embed(cache)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let (bod, bod_meta) = bod.embed(cache, env)?;
        let expr = cache.put_expr(Expr::Lam { info: *info, typ, bod })?;
        let meta =
          cache.put_expr_meta(ExprMeta::Lam(*pos, name, typ_meta, bod_meta))?;
        Ok((expr, meta))
      }
      Self::Pi(pos, name, info, typ, bod) => {
        let name = name.embed(cache)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let (bod, bod_meta) = bod.embed(cache, env)?;
        let expr = cache.put_expr(Expr::Pi { info: *info, typ, bod })?;
        let meta =
          cache.put_expr_meta(ExprMeta::Pi(*pos, name, typ_meta, bod_meta))?;
        Ok((expr, meta))
      }
      Self::Let(pos, name, typ, val, bod) => {
        let name = name.embed(cache)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let (val, val_meta) = val.embed(cache, env)?;
        let (bod, bod_meta) = bod.embed(cache, env)?;
        let expr = cache.put_expr(Expr::Let { typ, val, bod })?;
        let meta = cache.put_expr_meta(ExprMeta::Let(
          *pos, name, typ_meta, val_meta, bod_meta,
        ))?;
        Ok((expr, meta))
      }
      Self::Lit(pos, val) => {
        let val = val.embed(cache)?;
        let meta = cache.put_expr_meta(ExprMeta::Lit(*pos))?;
        let expr = cache.put_expr(Expr::Lit { val })?;
        Ok((expr, meta))
      }
      Self::Fix(pos, name, bod) => {
        let name = name.embed(cache)?;
        let (bod, bod_meta) = bod.embed(cache, env)?;
        let expr = cache.put_expr(Expr::Fix { bod })?;
        let meta = cache.put_expr_meta(ExprMeta::Fix(*pos, name, bod_meta))?;
        Ok((expr, meta))
      }
    }
  }

  // TODO: refactor to iterative version to avoid stack overflows
  pub fn unembed(
    cache: &mut Cache,
    expr: ExprCid,
    meta: ExprMetaCid,
  ) -> Result<Self, EmbedError> {
    let expr = cache.get_expr(&expr)?.clone();
    let meta = cache.get_expr_meta(&meta)?.clone();
    match (expr, meta) {
      (Expr::Var { idx }, ExprMeta::Var(pos)) => {
        let idx = idx.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        Ok(Self::BVar(pos, idx))
      }
      (Expr::Sort { univ }, ExprMeta::Sort(pos, univ_meta)) => {
        let univ = Universe::unembed(cache, univ, univ_meta)?;
        Ok(Self::Sort(pos, Box::new(univ)))
      }
      (
        Expr::Const { constant, levels },
        ExprMeta::Const(pos, name, constant_meta, levels_meta),
      ) => {
        // TODO: check constant is in cache, i.e. reference is defined
        let name = name::Name::unembed(cache, name)?;
        let mut ls = Vec::new();
        for (l, lm) in levels.iter().zip(levels_meta.iter()) {
          let level = Universe::unembed(cache, *l, *lm)?;
          ls.push(level)
        }
        Ok(Self::Const(pos, name, ls.into()))
      }
      (Expr::App { fun, arg }, ExprMeta::App(pos, fun_meta, arg_meta)) => {
        let fun = Self::unembed(cache, fun, fun_meta)?;
        let arg = Self::unembed(cache, arg, arg_meta)?;
        Ok(Self::App(pos, Box::new(fun), Box::new(arg)))
      }
      (
        Expr::Lam { info, typ, bod },
        ExprMeta::Lam(pos, name, typ_meta, bod_meta),
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let typ = Self::unembed(cache, typ, typ_meta)?;
        let bod = Self::unembed(cache, bod, bod_meta)?;
        Ok(Self::Lam(pos, name, info, Box::new(typ), Box::new(bod)))
      }
      (
        Expr::Pi { info, typ, bod },
        ExprMeta::Pi(pos, name, typ_meta, bod_meta),
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let typ = Self::unembed(cache, typ, typ_meta)?;
        let bod = Self::unembed(cache, bod, bod_meta)?;
        Ok(Self::Pi(pos, name, info, Box::new(typ), Box::new(bod)))
      }
      (
        Expr::Let { typ, val, bod },
        ExprMeta::Let(pos, name, typ_meta, val_meta, bod_meta),
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let typ = Self::unembed(cache, typ, typ_meta)?;
        let val = Self::unembed(cache, val, val_meta)?;
        let bod = Self::unembed(cache, bod, bod_meta)?;
        Ok(Self::Let(pos, name, Box::new(typ), Box::new(val), Box::new(bod)))
      }
      (Expr::Lit { val }, ExprMeta::Lit(pos)) => {
        let val = Literal::unembed(cache, val)?;
        Ok(Self::Lit(pos, val))
      }
      (Expr::Fix { bod }, ExprMeta::Fix(pos, name, bod_meta)) => {
        let name = name::Name::unembed(cache, name)?;
        let bod = Self::unembed(cache, bod, bod_meta)?;
        Ok(Self::Fix(pos, name, Box::new(bod)))
      }
      (expr, meta) => Err(EmbedError::Expr(expr, meta)),
    }
  }
}

impl Constant {
  pub fn embed_levels(
    cache: &mut Cache,
    levels: &Vector<name::Name>,
  ) -> Result<Vec<NameCid>, EmbedError> {
    let mut ls: Vec<NameCid> = Vec::new();
    let mut n: usize = 0;
    for l in levels {
      let l = l.embed(cache)?;
      ls.push(l);
    }
    Ok(ls)
  }

  pub fn embed(
    &self,
    cache: &mut Cache,
    env: &Env,
  ) -> Result<(ConstCid, ConstMetaCid), EmbedError> {
    match self {
      Self::Quotient { pos, name, level_params, typ, kind } => {
        let name = name.embed(cache)?;
        let levels = Constant::embed_levels(cache, level_params)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let cnst = cache.put_constant(Const::Quotient {
          levels: levels.len().into(),
          typ,
          kind: *kind,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Quotient {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
        })?;
        Ok((cnst, meta))
      }
      Self::Axiom { pos, name, level_params, typ, is_unsafe } => {
        let name = name.embed(cache)?;
        let levels = Constant::embed_levels(cache, level_params)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let cnst = cache.put_constant(Const::Axiom {
          levels: levels.len().into(),
          typ,
          is_unsafe: *is_unsafe,
          uid: name.0,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Axiom {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
        })?;
        Ok((cnst, meta))
      }
      Self::Theorem { pos, name, level_params, typ, val } => {
        let name = name.embed(cache)?;
        let levels = Constant::embed_levels(cache, level_params)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let (val, val_meta) = val.embed(cache, env)?;
        let cnst = cache.put_constant(Const::Theorem {
          levels: levels.len().into(),
          typ,
          val,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Theorem {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
          val_meta,
        })?;
        Ok((cnst, meta))
      }
      Self::Opaque { pos, name, level_params, typ, val, is_unsafe } => {
        let name = name.embed(cache)?;
        let levels = Constant::embed_levels(cache, level_params)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let (val, val_meta) = val.embed(cache, env)?;
        let cnst = cache.put_constant(Const::Opaque {
          levels: levels.len().into(),
          typ,
          val,
          is_unsafe: *is_unsafe,
          uid: name.0,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Opaque {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
          val_meta,
        })?;
        Ok((cnst, meta))
      }
      Self::Definition { pos, name, level_params, typ, val, safety } => {
        let name = name.embed(cache)?;
        let levels = Constant::embed_levels(cache, level_params)?;
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let (val, val_meta) = val.embed(cache, env)?;
        let cnst = cache.put_constant(Const::Definition {
          levels: levels.len().into(),
          typ,
          val,
          safety: *safety,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Definition {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
          val_meta,
        })?;
        Ok((cnst, meta))
      }
      Self::Inductive {
        pos,
        name,
        level_params,
        typ,
        intros,
        params,
        indices,
        is_unsafe,
      } => {
        let name = name.embed(cache)?;
        let levels = Constant::embed_levels(cache, level_params)?;
        let mut is = Vec::new();
        let mut intros_meta = Vec::new();
        use crate::content::constant::Intro;
        for i in intros {
          let ctor_name = (*i).ctor.embed(cache)?;
          let (typ, typ_meta) = i.typ.embed(cache, env)?;
          is.push(Intro { ctor: ctor_name, typ });
          intros_meta.push(typ_meta);
        }
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let cnst = cache.put_constant(Const::Inductive {
          levels: levels.len().into(),
          typ,
          params: (*params).into(),
          indices: (*indices).into(),
          intros: is,
          is_unsafe: *is_unsafe,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Inductive {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
          intros_meta,
        })?;
        Ok((cnst, meta))
      }
      Self::Constructor {
        pos,
        name,
        level_params,
        typ,
        induct,
        ctor_idx,
        params,
        fields,
        is_unsafe,
      } => {
        let name = name.embed(cache)?;
        let mut levels = Vec::new();
        let mut n: usize = 0;
        for l in level_params {
          let l = l.embed(cache)?;
          levels.push(l);
          n = n + 1;
        }
        let (typ, typ_meta) = typ.embed(cache, env)?;

        let induct_name = induct.embed(cache)?;
        let (induct_cid, induct_meta_cid) = env
          .constants
          .get(&induct_name)
          .ok_or(EmbedError::UndefinedConst(induct.clone(), induct_name))?;
        let cnst = cache.put_constant(Const::Constructor {
          levels: n.into(),
          typ,
          induct: *induct_cid,
          cidx: (*ctor_idx).into(),
          params: (*params).into(),
          fields: (*fields).into(),
          is_unsafe: *is_unsafe,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Constructor {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
          induct_meta: *induct_meta_cid,
        })?;
        Ok((cnst, meta))
      }
      Self::Recursor {
        pos,
        name,
        level_params,
        typ,
        induct,
        params,
        indices,
        motives,
        minors,
        rules,
        k,
        is_unsafe,
      } => {
        let name = name.embed(cache)?;
        let mut levels = Vec::new();
        let mut n: usize = 0;
        for l in level_params {
          let l = l.embed(cache)?;
          levels.push(l);
          n = n + 1;
        }
        let (typ, typ_meta) = typ.embed(cache, env)?;
        let induct_name = induct.embed(cache)?;
        let (induct_cid, induct_meta_cid) = env
          .constants
          .get(&induct_name)
          .ok_or(EmbedError::UndefinedConst(induct.clone(), induct_name))?;
        let mut rec_rules = Vec::new();
        let mut rec_rules_meta = Vec::new();
        use crate::content::constant::RecursorRule;
        for r in rules {
          let ctor_name = (*r).ctor.embed(cache)?;
          let (rhs, rhs_meta) = r.rhs.embed(cache, env)?;
          rec_rules.push(RecursorRule {
            ctor: ctor_name,
            fields: r.num_fields.into(),
            rhs,
          });
          rec_rules_meta.push(rhs_meta);
        }
        let cnst = cache.put_constant(Const::Recursor {
          levels: n.into(),
          typ,
          induct: *induct_cid,
          params: (*params).into(),
          indices: (*indices).into(),
          motives: (*motives).into(),
          minors: (*minors).into(),
          rules: rec_rules,
          k: *k,
          is_unsafe: *is_unsafe,
        })?;
        let meta = cache.put_const_meta(ConstMeta::Recursor {
          pos: *pos,
          name,
          levels_meta: levels,
          typ_meta,
          induct_meta: *induct_meta_cid,
          rules_meta: rec_rules_meta,
        })?;
        Ok((cnst, meta))
      }
    }
  }

  pub fn unembed_levels(
    cache: &mut Cache,
    len: BigUint,
    levels: &Vec<NameCid>,
  ) -> Result<Vector<name::Name>, EmbedError> {
    let mut ls = Vec::new();
    for l in levels {
      let n = name::Name::unembed(cache, *l)?;
      ls.push(n);
    }
    if BigUint::from(ls.len()) != len {
      Err(EmbedError::LevelLengthMismatch)
    }
    else {
      Ok(ls.into())
    }
  }

  // TODO: refactor to iterative version to avoid stack overflows
  pub fn unembed(
    cache: &mut Cache,
    cnst: ConstCid,
    meta: ConstMetaCid,
  ) -> Result<Self, EmbedError> {
    let cnst = cache.get_constant(&cnst)?.clone();
    let meta = cache.get_const_meta(&meta)?.clone();
    match (cnst, meta) {
      (
        Const::Quotient { levels, typ, kind },
        ConstMeta::Quotient { pos, name, levels_meta, typ_meta },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        Ok(Constant::Quotient { pos, name, level_params, typ, kind })
      }
      (
        Const::Axiom { levels, typ, is_unsafe, uid },
        ConstMeta::Axiom { pos, name, levels_meta, typ_meta },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        Ok(Constant::Axiom { pos, name, level_params, typ, is_unsafe })
      }
      (
        Const::Theorem { levels, typ, val },
        ConstMeta::Theorem { pos, name, levels_meta, typ_meta, val_meta },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        let val = Expression::unembed(cache, val, val_meta)?;
        Ok(Constant::Theorem { pos, name, level_params, typ, val })
      }
      (
        Const::Opaque { levels, typ, val, uid, is_unsafe },
        ConstMeta::Opaque { pos, name, levels_meta, typ_meta, val_meta },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        let val = Expression::unembed(cache, val, val_meta)?;
        Ok(Constant::Opaque { pos, name, level_params, typ, val, is_unsafe })
      }
      (
        Const::Definition { levels, typ, val, safety },
        ConstMeta::Definition { pos, name, levels_meta, typ_meta, val_meta },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        let val = Expression::unembed(cache, val, val_meta)?;
        Ok(Constant::Definition { pos, name, level_params, typ, val, safety })
      }
      (
        Const::Inductive { levels, typ, params, indices, intros, is_unsafe },
        ConstMeta::Inductive { pos, name, levels_meta, typ_meta, intros_meta },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        let mut is = Vec::new();
        use crate::constant::Intro;
        for (i, i_typ_meta) in intros.iter().zip(intros_meta.iter()) {
          let ctor = name::Name::unembed(cache, i.ctor)?;
          let typ = Expression::unembed(cache, i.typ, *i_typ_meta)?;
          is.push(Intro { ctor, typ });
        }
        let params =
          params.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        let indices =
          indices.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        Ok(Constant::Inductive {
          pos,
          name,
          level_params,
          typ,
          intros: is.into(),
          params,
          indices,
          is_unsafe,
        })
      }
      (
        Const::Constructor {
          levels,
          typ,
          induct,
          cidx,
          params,
          fields,
          is_unsafe,
        },
        ConstMeta::Constructor {
          pos,
          name,
          levels_meta,
          typ_meta,
          induct_meta,
        },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        let ind = Constant::unembed(cache, induct, induct_meta)?;
        let ind_name = match ind {
          Constant::Inductive { name, .. } => Ok(name),
          x => Err(EmbedError::UnexpectedInductive(x, induct, induct_meta)),
        }?;
        let params =
          params.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        let fields =
          fields.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        let ctor_idx =
          cidx.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        Ok(Constant::Constructor {
          pos,
          name,
          level_params,
          typ,
          params,
          fields,
          ctor_idx,
          induct: ind_name,
          is_unsafe,
        })
      }
      (
        Const::Recursor {
          levels,
          typ,
          induct,
          params,
          indices,
          motives,
          minors,
          rules,
          k,
          is_unsafe,
        },
        ConstMeta::Recursor {
          pos,
          name,
          levels_meta,
          typ_meta,
          induct_meta,
          rules_meta,
        },
      ) => {
        let name = name::Name::unembed(cache, name)?;
        let level_params =
          Constant::unembed_levels(cache, levels, &levels_meta)?;
        let typ = Expression::unembed(cache, typ, typ_meta)?;
        let ind = Constant::unembed(cache, induct, induct_meta)?;
        let ind_name = match ind {
          Constant::Inductive { name, .. } => Ok(name),
          x => Err(EmbedError::UnexpectedInductive(x, induct, induct_meta)),
        }?;
        let params =
          params.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        let indices =
          indices.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        let motives =
          motives.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        let minors =
          minors.try_into().map_err(|_| EmbedError::BigUintOverflow)?;
        use crate::constant::RecursorRule;
        let mut rs = Vec::new();
        for (r, rhs_meta) in rules.iter().zip(rules_meta.iter()) {
          let ctor = name::Name::unembed(cache, r.ctor)?;
          let rhs = Expression::unembed(cache, r.rhs, *rhs_meta)?;
          let num_fields = r
            .fields
            .clone()
            .try_into()
            .map_err(|_| EmbedError::BigUintOverflow)?;
          rs.push(RecursorRule { ctor, num_fields, rhs });
        }
        Ok(Constant::Recursor {
          pos,
          name,
          level_params,
          typ,
          params,
          indices,
          motives,
          minors,
          rules: rs.into(),
          induct: ind_name,
          is_unsafe,
          k,
        })
      }

      (cnst, meta) => Err(EmbedError::Const(cnst, meta)),
    }
  }
}

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

  #[quickcheck]
  fn expr_embed(expr: Expression) -> bool {
    println!("{:?}", expr);
    match expr_embed_inner(expr) {
      Ok(x) => x,
      Err(x) => {
        println!("{:?}", x);
        false
      }
    }
  }

  fn expr_embed_inner(expr: Expression) -> Result<bool, EmbedError> {
    let env = Env::new();
    let mut cache = Cache::new();
    let (cid, meta_cid) = expr.embed(&mut cache, &env)?;
    let expr2 = Expression::unembed(&mut cache, cid, meta_cid)?;
    Ok(expr == expr2)
  }
  //#[quickcheck]
  // fn constant_embed(constant: Constant) -> bool {
  //  println!("{:?}", constant);
  //  match constant_embed_inner(constant) {
  //    Ok(x) => x,
  //    Err(x) => {
  //      println!("{:?}", x);
  //      false
  //    }
  //  }
  //}

  // fn constant_embed_inner(constant: Constant) -> Result<bool, EmbedError> {
  //  let env = Env::new();
  //  let mut cache = Cache::new();
  //  let (cid, meta_cid) = constant.embed(&mut cache, &env)?;
  //  let constant2 = Constant::unembed(&mut cache, cid, meta_cid)?;
  //  Ok(constant == constant2)
  //}
}

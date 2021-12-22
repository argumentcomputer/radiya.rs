use crate::expression::Literal;

use crate::content::cid::{
  ConstCid,
  ConstMetaCid,
  ExprCid,
  ExprMetaCid,
  LitCid,
  NameCid,
  UnivCid,
  UnivMetaCid,
};

use crate::content::{
  constant::{
    Const,
    ConstMeta,
  },
  embed::{
    EmbedError,
    EmbedError::*,
  },
  expr::{
    Expr,
    ExprMeta,
  },
  name::Name,
  univ::{
    Univ,
    UnivMeta,
  },
};
use sp_std::collections::btree_map::BTreeMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cache {
  pub lit: BTreeMap<LitCid, Literal>,
  pub name: BTreeMap<NameCid, Name>,
  pub univ: BTreeMap<UnivCid, Univ>,
  pub univ_meta: BTreeMap<UnivMetaCid, UnivMeta>,
  pub expr: BTreeMap<ExprCid, Expr>,
  pub expr_meta: BTreeMap<ExprMetaCid, ExprMeta>,
  pub constant: BTreeMap<ConstCid, Const>,
  pub const_meta: BTreeMap<ConstMetaCid, ConstMeta>,
}

impl Cache {
  pub fn new() -> Self {
    Cache {
      lit: BTreeMap::new(),
      name: BTreeMap::new(),
      univ: BTreeMap::new(),
      univ_meta: BTreeMap::new(),
      expr: BTreeMap::new(),
      expr_meta: BTreeMap::new(),
      constant: BTreeMap::new(),
      const_meta: BTreeMap::new(),
    }
  }

  pub fn put_literal(&mut self, lit: Literal) -> Result<LitCid, EmbedError> {
    let cid = LitCid::new(&lit);
    self.lit.insert(cid, lit);
    Ok(cid)
  }

  pub fn get_literal(&mut self, cid: &LitCid) -> Result<&Literal, EmbedError> {
    self.lit.get(cid).ok_or(CacheGetLit(*cid))
  }

  pub fn put_name(&mut self, name: Name) -> Result<NameCid, EmbedError> {
    let cid = NameCid::new(&name);
    self.name.insert(cid, name);
    Ok(cid)
  }

  pub fn get_name(&mut self, cid: &NameCid) -> Result<&Name, EmbedError> {
    self.name.get(cid).ok_or(CacheGetName(*cid))
  }

  pub fn put_univ(&mut self, univ: Univ) -> Result<UnivCid, EmbedError> {
    let cid = UnivCid::new(&univ);
    self.univ.insert(cid, univ);
    Ok(cid)
  }

  pub fn get_univ(&mut self, cid: &UnivCid) -> Result<&Univ, EmbedError> {
    self.univ.get(cid).ok_or(CacheGetUniv(*cid))
  }

  pub fn put_univ_meta(
    &mut self,
    univ_meta: UnivMeta,
  ) -> Result<UnivMetaCid, EmbedError> {
    let cid = UnivMetaCid::new(&univ_meta);
    self.univ_meta.insert(cid, univ_meta);
    Ok(cid)
  }

  pub fn get_univ_meta(
    &mut self,
    cid: &UnivMetaCid,
  ) -> Result<&UnivMeta, EmbedError> {
    self.univ_meta.get(cid).ok_or(CacheGetUnivMeta(*cid))
  }

  pub fn put_expr(&mut self, expr: Expr) -> Result<ExprCid, EmbedError> {
    let cid = ExprCid::new(&expr);
    self.expr.insert(cid, expr);
    Ok(cid)
  }

  pub fn get_expr(&mut self, cid: &ExprCid) -> Result<&Expr, EmbedError> {
    self.expr.get(cid).ok_or(CacheGetExpr(*cid))
  }

  pub fn put_expr_meta(
    &mut self,
    expr_meta: ExprMeta,
  ) -> Result<ExprMetaCid, EmbedError> {
    let cid = ExprMetaCid::new(&expr_meta);
    self.expr_meta.insert(cid, expr_meta);
    Ok(cid)
  }

  pub fn get_expr_meta(
    &mut self,
    cid: &ExprMetaCid,
  ) -> Result<&ExprMeta, EmbedError> {
    self.expr_meta.get(cid).ok_or(CacheGetExprMeta(*cid))
  }

  pub fn put_constant(&mut self, lit: Const) -> Result<ConstCid, EmbedError> {
    let cid = ConstCid::new(&lit);
    self.constant.insert(cid, lit).map(|_| cid);
    Ok(cid)
  }

  pub fn get_constant(&mut self, cid: &ConstCid) -> Result<&Const, EmbedError> {
    self.constant.get(cid).ok_or(CacheGetConst(*cid))
  }

  pub fn put_const_meta(
    &mut self,
    lit: ConstMeta,
  ) -> Result<ConstMetaCid, EmbedError> {
    let cid = ConstMetaCid::new(&lit);
    self.const_meta.insert(cid, lit).map(|_| cid);
    Ok(cid)
  }

  pub fn get_const_meta(
    &mut self,
    cid: &ConstMetaCid,
  ) -> Result<&ConstMeta, EmbedError> {
    self.const_meta.get(cid).ok_or(CacheGetConstMeta(*cid))
  }
}

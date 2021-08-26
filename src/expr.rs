use crate::{
  level::Level,
  name::Name,
};

use sp_im::vector::Vector;

use sp_std::boxed::Box;

pub enum Bind {
  Default,
  Implicit,
  Strict,
  Class,
}

pub enum Expr {
  Var { idx: u64 },
  Lambda { name: Name, bind: Bind, dom: Box<Expr>, bod: Box<Expr> },
  Pi { name: Name, bind: Bind, dom: Box<Expr>, bod: Box<Expr> },
  Sort { level: Level },
  App { fun: Box<Expr>, arg: Box<Expr> },
  Let { name: Name, bind: Bind, typ: Box<Expr>, val: Box<Expr>, bod: Box<Expr> },
  Const { name: Name, levels: Vector<Level> },
  Local { name: Name, bind: Bind, typ: Box<Expr> },
}

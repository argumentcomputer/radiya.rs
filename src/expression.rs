use crate::{
  name::Name,
  universe::Univ,
};

use sp_im::vector::Vector;

use sp_std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Bind {
  Default,
  Implicit,
  Strict,
  Class,
}

#[derive(Clone, Debug)]
pub enum Expr {
  Var(u64),
  Lam(Name, Bind, Rc<Expr>, Rc<Expr>),
  All(Name, Bind, Rc<Expr>, Rc<Expr>),
  Sort(Univ),
  App(Rc<Expr>, Rc<Expr>),
  Let(Name, Bind, Rc<Expr>, Rc<Expr>, Rc<Expr>),
  Const(Name, Vector<Univ>),
}

#[cfg(test)]
pub mod tests {
  use crate::content::tests::frequency;

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for Bind {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Bind>)> = vec![
        (1, Box::new(|_| Bind::Default)),
        (1, Box::new(|_| Bind::Implicit)),
        (1, Box::new(|_| Bind::Strict)),
        (1, Box::new(|_| Bind::Class)),
      ];
      frequency(g, input)
    }
  }
}

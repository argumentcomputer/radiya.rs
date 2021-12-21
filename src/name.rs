use num_bigint::BigUint;
use num_traits::{
  One,
  Zero,
};
use sp_im::Vector;
use sp_std::fmt;

use alloc::string::{
  String,
  ToString,
};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum NamePart {
  Str(String),
  Int(BigUint),
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Name {
  pub parts: Vector<NamePart>,
}

impl Name {
  pub fn empty() -> Self { Name { parts: Vector::new() } }

  /// Simple shorthand for creating a Name
  pub fn simple(s: &[&str]) -> Self {
    Name { parts: s.iter().map(|s| NamePart::Str(s.to_string())).collect() }
  }

  pub fn append(&self, part: NamePart) -> Name {
    let mut new = self.clone();
    new.parts.push_back(part);
    new
  }

  pub fn print(&self) -> String {
    let mut res = String::new();
    let mut iter = self.parts.iter().peekable();
    if let Some(_) = iter.peek() {
      res.push('.');
    }
    while let Some(p) = iter.next() {
      if let Some(_) = iter.peek() {
        res.push('.');
      }
      match p {
        NamePart::Str(s) => res.push_str(s),
        NamePart::Int(n) => res.push_str(&format!("{}", n)),
      }
    }
    res
  }
}

impl fmt::Debug for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.print())
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.print())
  }
}

#[macro_export]
macro_rules! name {
  ( $ctx:expr ) => {
    Name::simple(&[$ctx])
  };
}

#[cfg(test)]
pub mod tests {
  use crate::tests::{
    arbitrary_big_uint,
    frequency,
    gen_range,
  };

  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  pub fn arbitrary_name_str(g: &mut Gen) -> NamePart {
    let s: String = Arbitrary::arbitrary(g);
    let mut s: String =
      s.chars().filter(|x| char::is_ascii_alphabetic(x)).collect();
    s.truncate(6);
    NamePart::Str(format!("_{}", s))
  }

  impl Arbitrary for NamePart {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> NamePart>)> = vec![
        (1, Box::new(|g| NamePart::Int(arbitrary_big_uint()(g)))),
        (1, Box::new(|g| arbitrary_name_str(g))),
      ];
      frequency(g, input)
    }
  }
  impl Arbitrary for Name {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut vec = Vec::new();
      let num = gen_range(g, 0..6);
      for _ in 0..num {
        vec.push(Arbitrary::arbitrary(g));
      }
      Name { parts: Vector::from(vec) }
    }
  }
}

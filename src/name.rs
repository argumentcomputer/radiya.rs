use num_bigint::BigUint;
use num_traits::{
  One,
  Zero,
};
use sp_im::Vector;
use sp_std::{
  borrow::Borrow,
  fmt,
  ops::Deref,
  rc::Rc,
  vec::Vec,
};

// use crate::export;

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

  pub fn from_unique(num: usize) -> Self {
    Name { parts: Vector::from(vec![NamePart::Int(num.into())]) }
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

/// Generates unique names
pub struct NameGenerator {
  prefix: Name,
  next_index: BigUint,
}

impl NameGenerator {
  pub fn new(prefix: Name) -> Self {
    NameGenerator { prefix, next_index: Zero::zero() }
  }

  pub fn next(&mut self) -> Name {
    let name = self.prefix.append(NamePart::Int(self.next_index.clone()));
    self.next_index = &self.next_index + BigUint::one();
    name
  }
}

#[macro_export]
macro_rules! name {
  ( $ctx:expr ) => {
    Name::simple(&[$ctx])
  };
}

// impl AsRef<str> for Name {
//  fn as_ref(&self) -> &str { self.inner.as_ref() }
//}
// impl<'a> From<&'a str> for Name {
//  fn from(v: &str) -> Name { Self { inner: Rc::from(v) } }
//}
// impl From<String> for Name {
//  fn from(v: String) -> Name { Self { inner: Rc::from(v) } }
//}

// impl Borrow<str> for Name {
//  fn borrow(&self) -> &str { self.inner.borrow() }
//}
// impl Deref for Name {
//  type Target = str;
//
//  fn deref(&self) -> &str { self.inner.deref() }
//}

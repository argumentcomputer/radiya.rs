use num_bigint::BigUint;
use sp_im::Vector;
use sp_std::{
  borrow::Borrow,
  fmt,
  ops::Deref,
  rc::Rc,
};

use alloc::string::{
  String,
  ToString,
};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum NamePart {
  Text(String),
  Int(BigUint),
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Name {
  pub system: bool,
  pub parts: Vector<NamePart>,
}

impl Name {
  pub fn print(&self) -> String {
    let mut res = String::new();
    if self.system {
      res.push_str("#_system");
    }
    let mut iter = self.parts.iter().peekable();
    if let Some(_) = iter.peek() {
      res.push('.');
    }
    while let Some(p) = iter.next() {
      if let Some(_) = iter.peek() {
        res.push('.');
      }
      match p {
        NamePart::Text(s) => res.push_str(s),
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
// pub fn is_valid_symbol_char(c: char) -> bool {
//  c != ':'
//    && c != ';'
//    && c != '('
//    && c != ')'
//    && c != '{'
//    && c != '}'
//    && c != ','
//    && c != '⁰'
//    && c != '¹'
//    && c != '˚'
//    && c != '⁺'
//    && !char::is_whitespace(c)
//    && !char::is_control(c)
//}
// pub fn is_valid_symbol_string(s: &str) -> bool {
//  let invalid_chars = s.starts_with('"')
//    || s.starts_with('\'')
//    || s.starts_with('#')
//    || s.chars().any(|x| !is_valid_symbol_char(x));
//  !s.is_empty() && !invalid_chars
//}

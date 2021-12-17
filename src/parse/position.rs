use crate::parse::span::Span;

use sp_cid::Cid;
use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  convert::TryInto,
  fmt,
};

use alloc::string::String;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Position {
  pub input: Cid,
  pub from_offset: u64,
  pub from_line: u64,
  pub from_column: u64,
  pub upto_offset: u64,
  pub upto_line: u64,
  pub upto_column: u64,
}

impl Position {
  pub fn range(self, input: String) -> String {
    let mut res = String::new();
    let gutter = format!("{}", self.upto_line).len();
    let pad = format!("{: >gutter$}", self.from_line, gutter = gutter).len()
      + 3
      + self.from_column as usize;
    res.push_str(&format!("{}▼\n", " ".to_owned().repeat(pad)));
    for (line_number, line) in input.lines().enumerate() {
      if ((line_number as u64 + 1) >= self.from_line)
        && ((line_number as u64 + 1) <= self.upto_line)
      {
        res.push_str(&format!(
          "{: >gutter$} | {}\n",
          line_number + 1,
          line,
          gutter = gutter
        ));
      }
    }
    let pad = format!("{: >gutter$}", self.upto_line, gutter = gutter).len()
      + 3
      + self.upto_column as usize;
    res.push_str(&format!("{}▲", " ".to_owned().repeat(pad)));
    res
  }

  pub fn from_upto(input: Cid, from: Span, upto: Span) -> Self {
    Self {
      input,
      from_offset: (from.location_offset() as u64),
      from_line: from.location_line() as u64,
      from_column: from.get_utf8_column() as u64,
      upto_offset: (upto.location_offset() as u64),
      upto_line: upto.location_line() as u64,
      upto_column: upto.get_utf8_column() as u64,
    }
  }
}

impl fmt::Display for Position {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}@{}:{}-{}:{}",
      self.input,
      self.from_line,
      self.from_column,
      self.upto_line,
      self.upto_column
    )
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;

  use crate::content::cid::{
    tests::arbitrary_cid,
    LITERAL,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for Position {
    fn arbitrary(g: &mut Gen) -> Self {
      Position {
        input: arbitrary_cid(g, LITERAL),
        from_offset: Arbitrary::arbitrary(g),
        from_line: Arbitrary::arbitrary(g),
        from_column: Arbitrary::arbitrary(g),
        upto_offset: Arbitrary::arbitrary(g),
        upto_line: Arbitrary::arbitrary(g),
        upto_column: Arbitrary::arbitrary(g),
      }
    }
  }
}

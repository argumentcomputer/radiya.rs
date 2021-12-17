use crate::parse::position::Position;

use alloc::borrow::ToOwned;
use sp_ipld::Ipld;

use crate::content::ipld::{
  IpldEmbed,
  IpldError,
};

impl IpldEmbed for Position {
  fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      Ipld::Link(self.input),
      self.from_offset.to_ipld(),
      self.from_line.to_ipld(),
      self.from_column.to_ipld(),
      self.upto_offset.to_ipld(),
      self.upto_line.to_ipld(),
      self.upto_column.to_ipld(),
    ])
  }

  fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    use Ipld::Link;
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Link(cid), from_offset, from_line, from_column, upto_offset, upto_line, upto_column] =>
        {
          let from_offset = u64::from_ipld(from_offset)?;
          let from_line = u64::from_ipld(from_line)?;
          let from_column = u64::from_ipld(from_column)?;
          let upto_offset = u64::from_ipld(upto_offset)?;
          let upto_line = u64::from_ipld(upto_line)?;
          let upto_column = u64::from_ipld(upto_column)?;
          Ok(Position {
            input: *cid,
            from_offset,
            from_line,
            from_column,
            upto_offset,
            upto_line,
            upto_column,
          })
        }
        xs => Err(IpldError::expected("Position", &Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::expected("Position", xs)),
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
  fn position_ipld(x: Position) -> bool {
    match Position::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}

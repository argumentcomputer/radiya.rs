#![cfg_attr(not(any(feature = "std", test)), no_std)]
#![cfg_attr(test, feature(new_uninit))]
#![cfg_attr(test, feature(box_into_inner))]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

#[macro_use]
extern crate alloc;

// pub mod declaration;
// pub mod inductive;
// pub mod quotient;
// pub mod notation;
pub mod content;
pub mod export;
pub mod expression;
pub mod name;
pub mod parse;
pub mod universe;

#[cfg(test)]
mod tests {
  use num_bigint::BigUint;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use sp_std::ops::Range;

  pub fn gen_range(g: &mut Gen, range: Range<usize>) -> usize {
    if range.end <= range.start {
      range.start
    }
    else {
      let res: usize = Arbitrary::arbitrary(g);
      (res % (range.end - range.start)) + range.start
    }
  }

  pub fn arbitrary_big_uint() -> Box<dyn Fn(&mut Gen) -> BigUint> {
    Box::new(move |g: &mut Gen| {
      let v: Vec<u8> = Arbitrary::arbitrary(g);
      BigUint::from_bytes_be(&v)
    })
  }
}

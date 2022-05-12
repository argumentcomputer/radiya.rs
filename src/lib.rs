#![cfg_attr(test, feature(new_uninit))]
#![cfg_attr(test, feature(box_into_inner))]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

extern crate alloc;

pub mod constant;
pub mod environment;
pub mod expression;
pub mod name;
pub mod nat;
pub mod parse;
pub mod universe;

// pub mod declaration;
// pub mod environment;
// pub mod local_context;
// pub mod inductive;
// pub mod quotient;
// pub mod export;
// pub mod parse;

//#[cfg(test)]
// mod tests {
//  use core::ops::Range;
//  use num_bigint::BigUint;
//  use quickcheck::{
//    Arbitrary,
//    Gen,
//  };
//
//  pub fn gen_range(g: &mut Gen, range: Range<usize>) -> usize {
//    if range.end <= range.start {
//      range.start
//    }
//    else {
//      let res: usize = Arbitrary::arbitrary(g);
//      (res % (range.end - range.start)) + range.start
//    }
//  }
//
//  pub fn arbitrary_big_uint() -> Box<dyn Fn(&mut Gen) -> BigUint> {
//    Box::new(move |g: &mut Gen| {
//      let v: Vec<u8> = Arbitrary::arbitrary(g);
//      BigUint::from_bytes_be(&v)
//    })
//  }
//}

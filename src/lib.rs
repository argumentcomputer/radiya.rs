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

pub mod constant;
pub mod environment;
pub mod local_context;
// pub mod inductive;
// pub mod quotient;
// pub mod notation;
pub mod content;
// pub mod export;
pub mod check;
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
  use rand::Rng;
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
  pub fn frequency<T, F: Fn(&mut Gen) -> T>(
    g: &mut Gen,
    gens: sp_std::vec::Vec<(i64, F)>,
  ) -> T {
    if gens.iter().any(|(v, _)| *v < 0) {
      panic!("Negative weight");
    }
    let sum: i64 = gens.iter().map(|x| x.0).sum();
    let mut rng = rand::thread_rng();
    let mut weight: i64 = rng.gen_range(1..=sum);
    // let mut weight: i64 = g.rng.gen_range(1, sum);
    for gen in gens {
      if weight - gen.0 <= 0 {
        return gen.1(g);
      }
      else {
        weight -= gen.0;
      }
    }
    panic!("Calculation error for weight = {}", weight);
  }

  pub fn next_case<T: Copy>(g: &mut Gen, gens: &Vec<(usize, T)>) -> T {
    let sum: usize = gens.iter().map(|x| x.0).sum();
    let mut weight: usize = gen_range(g, 1..sum);
    for gen in gens {
      match weight.checked_sub(gen.0) {
        None | Some(0) => {
          return gen.1;
        }
        _ => {
          weight -= gen.0;
        }
      }
    }
    panic!("Calculation error for weight = {}", weight);
  }
}

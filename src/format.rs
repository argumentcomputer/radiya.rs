pub mod decl;
pub mod expr;
pub mod ipld_embed;
pub mod name;
pub mod univ;

// pub const META_CODEC: u64 = 0x7EA4_4E7A_0000_0000;
pub const NAME_CODEC: u64 = 0x7EA4_4A4E_0000_0000;
pub const UNIV_CODEC: u64 = 0x7EA4_1E71_0000_0000;
pub const EXPR_CODEC: u64 = 0x7EA4_E712_0000_0000;
pub const NOTN_CODEC: u64 = 0x7EA4_4074_0000_0000;
pub const DECL_CODEC: u64 = 0x7EA4_DEC1_0000_0000;
// pub const CONS_CODEC: u64 = 0x7EA4_C042_0000_0000;
// pub const ENVR_CODEC: u64 = 0x7EA4_E414_0000_0000;

//#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// pub struct NotnCid {
//  pub hash: Multihash,
//}
//
//#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// pub struct MetaCid {
//  pub hash: Multihash,
//}
//
//#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// pub struct ExprCid {
//  pub hash: Multihash,
//}
//

#[cfg(test)]
pub mod tests {
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use sp_cid::Cid;
  use sp_multihash::{
    Code,
    MultihashDigest,
  };

  pub fn arbitrary_cid(g: &mut Gen, code: u64) -> Cid {
    let mut bytes: [u8; 32] = [0; 32];
    for x in bytes.iter_mut() {
      *x = Arbitrary::arbitrary(g);
    }
    Cid::new_v1(code, Code::Blake3_256.digest(&bytes))
  }
}

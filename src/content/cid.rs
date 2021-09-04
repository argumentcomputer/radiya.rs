use sp_cid::Cid;
use sp_multihash::Multihash;

const NAME_SPACE: u64 = 0x6c71c4;
const UNIV_SPACE: u64 = 0x268bd2;
const EXPR_SPACE: u64 = 0x2aa198;
const DECL_SPACE: u64 = 0x859900;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnivCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprCid(pub Cid);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeclCid(pub Cid);

impl NameCid {
  pub fn new(hash: Multihash) -> Self { NameCid(Cid::new_v1(NAME_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Option<Self> {
    if cid.codec() == NAME_SPACE { Some(NameCid(cid)) } else { None }
  }
}

impl UnivCid {
  pub fn new(hash: Multihash) -> Self { UnivCid(Cid::new_v1(UNIV_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Option<Self> {
    if cid.codec() == UNIV_SPACE { Some(UnivCid(cid)) } else { None }
  }
}

impl ExprCid {
  pub fn new(hash: Multihash) -> Self { ExprCid(Cid::new_v1(EXPR_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Option<Self> {
    if cid.codec() == EXPR_SPACE { Some(ExprCid(cid)) } else { None }
  }
}

impl DeclCid {
  pub fn new(hash: Multihash) -> Self { DeclCid(Cid::new_v1(DECL_SPACE, hash)) }

  pub fn from_cid(cid: Cid) -> Option<Self> {
    if cid.codec() == DECL_SPACE { Some(DeclCid(cid)) } else { None }
  }
}

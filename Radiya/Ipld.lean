import Radiya.UnsignedVarInt
import Radiya.Multihash
import Radiya.Cid
import Std.Data.RBTree

open Std (RBNode)

--abbrev ByteVector n := { a: ByteArray // a.size = n }

  --digest : { a : ByteArray // a.size = size - (size / 8)}

inductive Ipld where
| null
| bool (b : Bool)
| number (n : UInt64)
| string (s : String)
| byte (b : ByteArray)
| array (elems : Array Ipld)
| object (kvPairs : RBNode String (fun _ => Ipld))
| link (cid: Cid)
deriving Inhabited



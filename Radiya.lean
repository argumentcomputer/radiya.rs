import Radiya.Cid
import Radiya.Multihash
import Radiya.Ipld
import Blake3

def main : IO Unit :=
  println! "Hello, world! {Blake3.hash "hello".toByteArray}"

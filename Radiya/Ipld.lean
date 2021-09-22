import Std.Data.RBTree

open Std (RBNode)

namespace Nat

  def toByteArrayCore : Nat → Nat → ByteArray → ByteArray
  | 0, n, bytes => bytes
  | fuel+1, n, bytes =>
      let b: UInt8 := UInt8.ofNat (n % 256);
      let n' := n / 256;
      if n' = 0 then (bytes.push b)
      else toByteArrayCore fuel n' (bytes.push b)

  def toByteArrayLE (n: Nat) : ByteArray := 
    toByteArrayCore (n+1) n { data := #[] }

  def toByteArrayBE (n: Nat) : ByteArray := do
    let bytes := (toByteArrayCore (n+1) n { data := #[]})
    let mut bytes' : ByteArray := { data := #[]}
    for i in [0:bytes.size] do
      bytes' := bytes'.push (bytes.data[bytes.size - 1 - i])
    bytes'

  def lenBytes (n : Nat) : Nat := n.toByteArrayLE.size

  def fromByteArrayLE (b: ByteArray) : Nat := do
    let mut x := 0
    for i in [:b.size] do
      x := x + Nat.shiftLeft (UInt8.toNat b.data[i]) (i * 8)
    return x

  def fromByteArrayBE (b: ByteArray) : Nat := do
    let mut x := 0
    for i in [:b.size] do
      x := Nat.shiftLeft x 8 + (UInt8.toNat b.data[i])
    return x

end Nat

#check Nat.fromByteArrayLE (Nat.toByteArrayLE 12345678910) = 12345678910
#check Nat.fromByteArrayBE (Nat.toByteArrayBE 12345678910) = 12345678910

def toVarIntCore : Nat → Nat → ByteArray → ByteArray
| 0, n, bytes => bytes
| fuel+1, n, bytes =>
      let b: UInt8 := UInt8.ofNat (n % 128);
      let n' := n / 128;
      if n' = 0 then (bytes.push b)
      else toVarIntCore fuel n' (bytes.push (b + 128))

def toVarInt (n: Nat) : ByteArray := 
  toVarIntCore (n+1) n { data := #[] }

def fromVarInt (b: ByteArray) : Nat := do
  let mut x := 0
  for i in [:b.size] do
    x := x + Nat.shiftLeft (UInt8.toNat b.data[i] % 128) (i * 7)
  return x

#check (toVarInt 1 = { data := #[0b00000001] })
#check fromVarInt (toVarInt 1) = 1
#check (toVarInt 127 = { data := #[0b01111111] })
#check fromVarInt (toVarInt 127) = 127
#check (toVarInt 255 = { data := #[0b01111111, 0b00000001] })
#check fromVarInt (toVarInt 255) = 255
#check (toVarInt 300 = { data := #[0b10101100, 0b00000010] })
#check fromVarInt (toVarInt 300) = 300
#check (toVarInt 16384 = { data := #[0b10000000, 0b10000000, 0b00000001] })
#check fromVarInt (toVarInt 16384) = 16384

abbrev ByteVector n := { a: ByteArray // a.size = n }

structure Multihash where
  code : Nat
  size : Nat
  digest : ByteArray
  --digest : { a : ByteArray // a.size = size - (size / 8)}

structure Cid where
  version : Nat
  codec: Nat
  hash: Multihash

inductive Ipld where
| null
| bool (b : Bool)
| num (n : UInt64)
| str (s : String)
| arr (elems : Array Ipld)
| obj (kvPairs : RBNode String (fun _ => Ipld))
| link (cid: Cid)
deriving Inhabited



import Radiya.UnsignedVarInt

structure Multihash where
  code : Nat
  size : Nat
  digest : ByteArray
  deriving BEq, Inhabited

namespace Multihash

def toBytes (self : Multihash) : ByteArray :=
  ByteArray.append (toVarInt self.code) $
  ByteArray.append (toVarInt self.size) $
  self.digest

inductive Error
| BadDigestSize (x: Nat) (y: Nat)
deriving BEq

instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq a b := match a, b with
  | Except.ok a, Except.ok b => a == b
  | Except.error a, Except.error b => a == b
  | _, _ => false

def fromBytes (bytes : ByteArray) : Except Error Multihash := do
  let code := fromVarInt ( { data := bytes.data[0:1].toArray })
  let size := fromVarInt ( { data := bytes.data[1:2].toArray })
  let digest := { data := bytes.data[2:].toArray }
  if bytes.data.size > size + 2 
  then Except.error (Error.BadDigestSize bytes.data.size size) 
  else return { code := code, size := size, digest := digest }


private def example1 : Multihash := { code := 0x11, size := 0x4,  digest := { data := #[0b10110110, 0b11111000, 0b01011100, 0b10110101]}}

#eval 0b101101100
#eval (toBytes example1) 
#eval #[0b00010001, 0b00000100, 0b10110110, 0b11111000, 0b01011100, 0b10110101]

#eval (toBytes example1) == { data := #[0b0001001, 0b00000100, 0b10110110, 0b11111000, 0b01011100, 0b10110101] }

#eval (pure example1) == fromBytes (toBytes example1)

end Multihash
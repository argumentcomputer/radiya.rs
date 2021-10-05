import Radiya.UnsignedVarInt

inductive Multibase
--| identity
| base2
| base8
| base10
| base16
| base16upper
| base32hex
| base32hexupper
| base32hexpad
| base32hexpadupper
| base32
| base32upper
| base32pad
| base32padupper
| base32z
| base36
| base36upper
| base58btc
| base58flickr
| base64
| base64pad
| base64url
| base64urlpad
--| proquint

namespace Multibase

def toCode : Multibase → Char 
--| identity => '\x00'
| base2 => '0'
| base8 => '7'
| base10 => '9'
| base16 => 'f'
| base16upper => 'F'
| base32hex => 'v'
| base32hexupper => 'V'
| base32hexpad => 't'
| base32hexpadupper => 'T'
| base32 => 'b'
| base32upper => 'B'
| base32pad => 'c'
| base32padupper => 'C'
| base32z => 'h'
| base36 => 'k'
| base36upper => 'K'
| base58btc => 'z'
| base58flickr => 'Z'
| base64 => 'm'
| base64pad => 'M'
| base64url => 'u'
| base64urlpad => 'U'
--| proquint => 'p'

def fromCode : Char -> Option Multibase
--| '\x00' => Option.some identity 
| '0' => Option.some base2 
| '7' => Option.some base8 
| '9' => Option.some base10 
| 'f' => Option.some base16 
| 'F' => Option.some base16upper 
| 'v' => Option.some base32hex 
| 'V' => Option.some base32hexupper 
| 't' => Option.some base32hexpad 
| 'T' => Option.some base32hexpadupper 
| 'b' => Option.some base32 
| 'B' => Option.some base32upper 
| 'c' => Option.some base32pad 
| 'C' => Option.some base32padupper 
| 'h' => Option.some base32z 
| 'k' => Option.some base36 
| 'K' => Option.some base36upper 
| 'z' => Option.some base58btc 
| 'Z' => Option.some base58flickr 
| 'm' => Option.some base64 
| 'M' => Option.some base64pad 
| 'u' => Option.some base64url 
| 'U' => Option.some base64urlpad 
--| 'p' => Option.some proquint 
| _ => Option.none

  namespace Alphabet
    def base2: String := "01"
    def base8: String := "01234567"
    def base10: String := "0123456789"
    def base16: String := "0123456789abcdef"
    def base16upper: String := "0123456789ABCDEF"
    def base32: String := "abcdefghijklmnopqrstuvwxyz234567"
    def base32upper: String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
    def base32hex : String := "0123456789abcdefghijklmnopqrstuv"
    def base32hexupper : String := "0123456789ABCDEFGHIJKLMNOPQRSTUV"
    def base32z : String := "ybndrfg8ejkmcpqxot1uwisza345h769"
    def base36 : String := "0123456789abcdefghijklmnopqrstuvwxyz"
    def base36upper : String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    def base58flickr : String := 
      "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
    def base58btc : String := 
      "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    def base64 : String := 
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    def base64url : String := 
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  end Alphabet

def alphabet : Multibase → String
| base2 => Alphabet.base2
| base8 => Alphabet.base8
| base10 => Alphabet.base10
| base16 => Alphabet.base16
| base16upper => Alphabet.base16upper
| base32hex => Alphabet.base32hex
| base32hexupper => Alphabet.base32hexupper
| base32hexpad => Alphabet.base32hex
| base32hexpadupper => Alphabet.base32hexupper
| base32 => Alphabet.base32
| base32upper => Alphabet.base32upper
| base32pad => Alphabet.base32
| base32padupper => Alphabet.base32upper
| base32z => Alphabet.base32z
| base36 => Alphabet.base36
| base36upper => Alphabet.base36upper
| base58btc => Alphabet.base58btc
| base58flickr => Alphabet.base58flickr
| base64 => Alphabet.base64
| base64pad => Alphabet.base64
| base64url => Alphabet.base64url
| base64urlpad => Alphabet.base64url

def encodeCore (alpha : String) : Nat → Nat → String → String
| 0, n, string => string
| fuel+1, n, string =>
    let base := alpha.length
    let char: String := String.singleton $ alpha.get (n % base);
    let n' := n / base;
    if n' = 0 then (String.append char string)
    else encodeCore alpha fuel n' (String.append char string)

def pad64 (input : String) : String :=
  let pad := (List.replicate ((4 - (input.length % 4)) % 4) '=').asString
  input.append pad

def encodeBase64 (pad: Bool) (input : ByteArray) : String := do
  let x := ByteArray.size input % 3
  let mut bytes := input
  let mut str := ""
  if x == 1 then bytes := bytes.append [0x00, 0x00].toByteArray
  if x == 2 then bytes := bytes.append [0x00].toByteArray
  for i in [:(bytes.size / 3)] do
    let b0 := bytes.data[3 * i]
    let b1 := bytes.data[3 * i + 1]
    let b2 := bytes.data[3 * i + 2]
    let s0 := b0.shiftRight 2
    let s1 := UInt8.xor
      ((b0.land 0b00000011).shiftLeft 4) 
      ((b1.land 0b11110000).shiftRight 4)
    let s2 := UInt8.xor
      ((b1.land 0b00001111).shiftLeft 2) 
      ((b2.land 0b11000000).shiftRight 6)
    let s3 := b2.land 0b00111111
    str := str.push (Alphabet.base64.get s0.toNat)
    str := str.push (Alphabet.base64.get s1.toNat)
    str := str.push (Alphabet.base64.get s2.toNat)
    str := str.push (Alphabet.base64.get s3.toNat)
  if pad then do
    if x == 1 then 
      str := str.set (str.length - 1) '='
      str := str.set (str.length - 2) '='
    if x == 2 then 
      str := str.set (str.length - 1) '='
    return str
  else 
    if x == 1 then str := str.dropRight 2
    if x == 2 then str := str.dropRight 1
    return str

--def decodeBase64 (input: String) : ByteArray := do
--  let mut str := input
--  let mut bytes := ByteArray.empty
--  bytes

def pad32 (input : String) : String :=
  let pad := (List.replicate ((8 - (input.length % 8)) % 8) '=').asString
  input.append pad

def padBin (input : String) : String :=
  let pad := (List.replicate ((8 - (input.length % 8)) % 8) '0').asString
  pad.append input

def countZeros (input : ByteArray) : Nat := 
  (List.takeWhile (fun x => x.toNat == 0) input.data.data).length

def padZeros (alpha: String) (n: Nat) (input : String) : String := {data := List.replicate n alpha[0]} ++ input

-- Todo: left zeros, base8, String <-> ByteArray
def encode (base: Multibase) (input : ByteArray) : String :=
  if input == ByteArray.empty then return "" else
  let x := Nat.fromByteArrayBE input
  let zs := countZeros input
  let code := String.singleton base.toCode
  let alphabet := base.alphabet
  let core := encodeCore alphabet (x+1) x ""
  String.append code $ 
  match base with
  | base2 => padBin core
  | base8 => core
  | base10 => padZeros alphabet zs $ core
  | base16 => core
  | base16upper => core
  | base32hex => core
  | base32hexupper => core
  | base32hexpad => pad32 $ core
  | base32hexpadupper => pad32 $ core
  | base32 => core
  | base32upper => core
  | base32pad => pad32 $ core
  | base32padupper => pad32 $ core
  | base32z => core
  | base36 => core
  | base36upper => core
  | base58btc => core
  | base58flickr => core
  | base64 => core
  | base64pad => pad64 $ core
  | base64url => core
  | base64urlpad => pad64 $ core


  namespace Test
    private def base2ex1 : String := encode base2 ({ data := #[0x58, 0x59, 0x5a]})
    #eval base2ex1 == "0010110000101100101011010"
    private def base2ex2 : String := encode base2 ({ data := #[0x1a]})
    #eval base2ex2 == "000011010"

    private def base10ex1 : String := encode base10 ({ data := #[0x00, 0x01]})
    #eval base10ex1 == "901"
    private def base10ex2 : String := encode base10 ({ data := #[0x00, 0x00, 0xff]})
    #eval base10ex2 == "900255"
    private def base10ex3 : String := encode base10 ({ data := #[0x01, 0x00]})
    #eval base10ex3 == "9256"
    private def base10ex4 : String := encode base10 ({ data := #[0x00, 0x01, 0x00]})
    #eval base10ex4 == "90256"

    -- basic.csv
    def basic : ByteArray := "yes mani !".toUTF8
    #eval basic == ByteArray.mk #[0x79, 0x65, 0x73, 0x20, 0x6D, 0x61, 0x6E, 0x69, 0x20, 0x21]

    #eval encode base2 basic == 
      "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
    #eval encode base8 basic -- == "7362625631006654133464440102"

    #eval encode base10            basic == "9573277761329450583662625"
    #eval encode base16            basic == "f796573206d616e692021"
    #eval encode base16upper       basic == "F796573206D616E692021"
    #eval encode base32            basic == "bpfsxgidnmfxgsibb"
    #eval encode base32upper       basic == "BPFSXGIDNMFXGSIBB"
    #eval encode base32hex         basic == "vf5in683dc5n6i811"
    #eval encode base32hexupper    basic == "VF5IN683DC5N6I811"
    #eval encode base32pad         basic == "cpfsxgidnmfxgsibb"
    #eval encode base32padupper    basic == "CPFSXGIDNMFXGSIBB"
    #eval encode base32hexpad      basic == "tf5in683dc5n6i811"
    #eval encode base32hexpadupper basic == "TF5IN683DC5N6I811"
    #eval encode base32z           basic == "hxf1zgedpcfzg1ebb"
    #eval encode base36            basic == "k2lcpzo5yikidynfl"
    #eval encode base36upper       basic == "K2LCPZO5YIKIDYNFL"
    #eval encode base58flickr      basic == "Z7Pznk19XTTzBtx"
    #eval encode base58btc         basic == "z7paNL19xttacUY"
    #eval encode base64            basic -- == "meWVzIG1hbmkgIQ"
    #eval encode base64pad         basic -- == "MeWVzIG1hbmkgIQ=="
    #eval encode base64url         basic -- == "ueWVzIG1hbmkgIQ"
    #eval encode base64urlpad      basic -- == "UeWVzIG1hbmkgIQ=="

    #eval "MA".toUTF8
    #eval encode base64 {data := #[0x4d, 0x61, 0x00] }
    #eval Nat.fromByteArrayBE "MA".toUTF8
    #eval encode base64 "Man".toUTF8
    #eval encode base64 "yes".toUTF8
    #eval encode base64 "A".toUTF8 -- == mQQ
    #eval encode base64 "AA".toUTF8 -- == mQUE
    #eval encode base64 "AAA".toUTF8 -- == mQUFB



    #eval encode base16 "hello world".toUTF8 == "f68656c6c6f20776f726c64"
    #eval encode base16upper "hello world".toUTF8 == "F68656C6C6F20776F726C64"
    #eval encode base32 "hello world".toUTF8 --== "bnbswy3dpeB3W64TMMQ"
    #eval encode base32upper "hello world".toUTF8 -- == "F68656C6C6F20776F726C64"
    #eval encode base36 "hello world".toUTF8 == "kfuvrsivvnfrbjwajo"

    

  end Test

end Multibase

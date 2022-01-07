-- import Ipld
import Lean

open Lean
open System

def main (args : List String) : IO UInt32 := do
  try
    let file := FilePath.mk <| args.get! 0 
    let env ← Lean.mkEmptyEnvironment
    -- read and print a file
    let synt ← Parser.testParseFile env file
    let src ← IO.FS.readFile file
    IO.println src
    -- test
    pure 0
  catch e =>
    IO.eprintln <| "Error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1


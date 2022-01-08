import Ipld
import Lean

open Lean
open Lean.Elab
open System

namespace Radiya
def runFrontend (input : String) (opts : Options) (fileName : String) (mainModuleName : Name) (trustLevel : UInt32 := 0) : IO (Environment × Bool) := do
  let inputCtx := Parser.mkInputContext input fileName
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header opts messages inputCtx trustLevel
  let env := env.setMainModule mainModuleName
  let s ← IO.processCommands inputCtx parserState (Command.mkState env messages opts)
  for msg in s.commandState.messages.toList do
    IO.print (← msg.toString (includeEndPos := getPrintMessageEndPos opts))
  pure (s.commandState.env, !s.commandState.messages.hasErrors)

end Radiya

def main (args : List String) : IO UInt32 := do
  try
    let file := FilePath.mk <| args.get! 0 
    let env ← Lean.mkEmptyEnvironment
    -- read and print a file
    let options := {}
    let mainModule ← moduleNameOfFileName file none
    let trustLevel := 0
    let src ← IO.FS.readFile file
    let (env, isErr) ← Radiya.runFrontend src options file.toString mainModule trustLevel
    -- let synt ← Parser.testParseFile env file
    IO.println src
    -- test
    pure 0
  catch e =>
    IO.eprintln <| "Error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1

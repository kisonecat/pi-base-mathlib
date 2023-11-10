import PiBase
import PiBase.Property

import Lean
import Cli

open Lean Cli

unsafe def runVerifyCmd (p : Parsed) : IO UInt32 := do
  let rootPath : System.FilePath := "data"
  let propertiesPath := rootPath.join "properties"
  let props ← PiBase.readProperties propertiesPath
  
  Std.HashMap.forM (fun k v => do
    if let some m := v.mathlib then
      IO.println s!"{k} := {m}"
  ) props
  
  return 0

unsafe def verify := `[Cli|
  verify VIA runVerifyCmd;
  "verify ."
]

unsafe def runPiBaseCmd (p : Parsed) : IO UInt32 := do
  p.printHelp
  return 0

unsafe def piBaseCmd : Cmd := `[Cli|
  pibase VIA runPiBaseCmd; ["0.0.1"]
  "Validate formal proofs referenced by https://github.com/pi-base"

  FLAGS:
    "data" : String; "Path to https://github.com/pi-base/data"

  SUBCOMMANDS:
    verify
]

unsafe def main (args : List String) : IO UInt32 :=
  piBaseCmd.validate args

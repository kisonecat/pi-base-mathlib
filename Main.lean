import PiBase
import PiBase.Property
import PiBase.Theorem
import PiBase.Verify

import Lean
import Cli

open Lean Cli

unsafe def runVerifyCmd (p : Parsed) : IO UInt32 := do
  let rootPath : System.FilePath := "data"
  let propertiesPath := rootPath.join "properties"
  let theoremsPath := rootPath.join "theorems"
  let props ← PiBase.readProperties propertiesPath

  -- Std.HashMap.forM (fun k v => do
  --   if let some m := v.mathlib then
  --     IO.println s!"{k} := {m}"
  --     IO.println (← PiBase.findDocumentationURL m)
  -- ) props

  let theorems ← PiBase.readTheorems theoremsPath

  Std.HashMap.forM (fun k v => do
    if let some m := v.mathlib then
      match (← PiBase.verifyTheorem v props) with
      | Except.ok _ => IO.println s!"{k} := {m}"
      | Except.error s => IO.println s!"{k}: {s}"
  ) theorems

  return 0

unsafe def verify := `[Cli|
  verify VIA runVerifyCmd;
  "verify"
]

unsafe def runDumpCmd (p : Parsed) : IO UInt32 := do
  let rootPath : System.FilePath := "data"
  let propertiesPath := rootPath.join "properties"
  let theoremsPath := rootPath.join "theorems"
  let props ← PiBase.readProperties propertiesPath

  Std.HashMap.forM (fun k v => do
     if let some m := v.mathlib then
       IO.println ""
       if let some n := v.name then
         IO.println s!"-- {n}"
       IO.println s!"abbrev {k} := {m}"
  ) props

  let theorems ← PiBase.readTheorems theoremsPath

  IO.println ""
  IO.println "variable {X : Type*} [TopologicalSpace X]"
  IO.println ""

  Std.HashMap.forM (fun k v => do
    if PiBase.propertiesPresent (v.hypothesis) props then
      if PiBase.propertiesPresent (v.conclusion) props then
        IO.println ""
        IO.println s!"theorem {k} : {PiBase.renderHypothesis v.hypothesis} → ({PiBase.renderConclusion v.conclusion}) := by sorry"
  ) theorems

  return 0

unsafe def dump := `[Cli|
  dump VIA runDumpCmd;
  "dump"
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
    verify;
    dump
]

unsafe def main (args : List String) : IO UInt32 :=
  piBaseCmd.validate args

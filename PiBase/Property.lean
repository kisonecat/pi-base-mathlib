import Lean.Data.Parsec
import Lean.Expr
import Lean.Meta
import Lean.Environment
import Mathlib

import PiBase.Reference
import PiBase.Logging

import Std.Lean.Util.Path

namespace PiBase

open Lean Core Elab Command Std.Tactic.Lint

structure Property where
  uid : Nat
  name : Option String := none
  aliases : List String := []
  refs : List Reference := []
  text : String := ""
  mathlib : Option String := none
deriving Repr

open Property

open Lean.Parsec

private def parseText : Lean.Parsec (Property → Property) := attempt do
  skipString "---\n"
  let t ← manyChars anyChar
  pure $ λ p => { p with text := t }

private def parseAlias : Lean.Parsec String := do
  ws
  skipChar '-'
  ws
  let n ← manyChars (satisfy (fun c => c != '\n'))
  skipChar '\n'
  pure $ n

private def parseReferences' : Lean.Parsec (Property → Property) := attempt do
  let xs ← parseReferences
  pure $ λ p => { p with refs := xs }

private def parseAliases : Lean.Parsec (Property → Property) := attempt do
  skipString "aliases:"
  ws
  let xs ← many $ parseAlias
  pure $ λ p => { p with aliases := xs.toList }

private def parseName : Lean.Parsec (Property → Property) := attempt do
  skipString "name:"
  ws
  let n ← manyChars (satisfy (· != '\n'))
  skipChar '\n'
  pure $ λ p => { p with name := some n }

private def parseMathlib : Lean.Parsec (Property → Property) := attempt do
  skipString "mathlib:"
  ws
  let n ← manyChars (satisfy (fun c => c != '\n'))
  skipChar '\n'
  pure $ λ p => { p with mathlib := some n }

private def parseUid : Lean.Parsec Property := do
  skipString "uid:"
  ws
  skipChar 'P'
  let d ← manyChars digit
  skipChar '\n'

  match d.toNat? with
  | none => λ it => ParseResult.error it s!"could not parse number {d}"
  | some i => λ it => ParseResult.success it { uid := i : Property }

private def propertyParser : Lean.Parsec Property := do
  Lean.Parsec.skipString "---\n"
  let p ← parseUid
  let fs ← many (parseAliases <|> parseName <|> parseReferences' <|> parseText <|> parseMathlib)
  pure $ fs.foldl (fun x f => f x) p

-- Function to read the content of a single file into a string
private def readFileContents (path : System.FilePath) : IO (Except String Property) := do
  IO.FS.withFile path IO.FS.Mode.read fun handle => do
    let s ← handle.readToEnd
    let result := Lean.Parsec.run propertyParser s
    pure result

unsafe def isValidProperty ( name : String ) : IO (Except String Unit) := do
  Lean.searchPathRef.set compile_time_search_path%
  Lean.withImportModules #[`Mathlib] {} (trustLevel := 1024) (fun env => do
    let state := {}
    let sCore : Lean.Core.State := {env}
    let ctx := {fileName := "", fileMap := default}
    let mctx : Lean.Meta.Context := {}

    let prop := Lean.Expr.const (Lean.Name.mkSimple name) [.param `u]

    try
      let m : MetaM Bool := do
        let type := Lean.Expr.sort (.succ (.param `u))
        let x ← Lean.Meta.mkFreshExprMVar (some type)

        let spaceType := Lean.Expr.app (Lean.Expr.const ``TopologicalSpace [.param `u]) x
        let spaceX ← Lean.Meta.mkFreshExprMVar (some spaceType)

        let e := Lean.Expr.app (Lean.Expr.app prop x) spaceX

        pure $ (← Lean.Meta.inferType e).isProp

      let result ← Lean.Meta.MetaM.toIO m ctx sCore mctx state

      pure $ if result.1 then Except.ok () else Except.error "not Prop"
    catch ex =>
      pure $ Except.error ex.toString
  )

private def paddedUid (p : Property) : String :=
  let str := toString p.uid
  let paddingNeeded := max 0 (6 - str.length)
  let padding := "".pushn '0' paddingNeeded
  "P" ++ padding ++ str

unsafe def readProperties (path : System.FilePath) : IO (Std.HashMap String Property) := do
  let entries <- path.readDir
  let mut result := Std.mkHashMap entries.size

  for entry in entries do
    match (← readFileContents $ path.join entry.fileName) with
    | Except.error e => reportError entry.fileName e
    | Except.ok p =>
      if (paddedUid p) ++ ".md" != entry.fileName then
        reportError entry.fileName s!"uid {paddedUid p} does not agree with filename {entry.fileName}"
      else
        if let some m := p.mathlib then
          match (← isValidProperty m) with
          | Except.error e => reportError entry.fileName e
          | Except.ok _ => result := result.insert (paddedUid p) p
        else
          reportWarning entry.fileName "missing mathlib"

  pure result

end PiBase

import Mathlib

import Lean.Data.Parsec
import Lean.Expr
import Lean.Meta

import PiBase.Reference
import PiBase.Logging

namespace PiBase

open Lean Core Elab Command

structure Theorem where
  uid : Nat
  refs : List Reference := []
  converses : List String := []
  text : String := ""
  hypothesis : List Lean.Expr := [Lean.Expr.bvar 0]
  conclusion : Lean.Expr := Lean.Expr.bvar 0
  mathlib : Option String := none
deriving Repr

open Theorem

open Lean.Parsec

private def parseText : Lean.Parsec (Theorem → Theorem) := attempt do
  skipString "---\n"
  let t ← manyChars anyChar
  pure $ λ p => { p with text := t }

private def parseReferences' : Lean.Parsec (Theorem → Theorem) := attempt do
  let xs ← parseReferences
  pure $ λ p => { p with refs := xs }

private def parseTheoremId : Lean.Parsec String := attempt do
  skipString "T"
  let t <- manyChars digit
  pure $ "T" ++ t

private def parseMathlib : Lean.Parsec (Theorem → Theorem) := attempt do
  skipString "mathlib:"
  ws
  let n ← manyChars (satisfy (fun c => c != '\n'))
  skipChar '\n'
  pure $ λ p => { p with mathlib := some n }

private def parseConverse : Lean.Parsec String := attempt do
  ws
  skipString "-"
  ws
  let v ← parseTheoremId
  ws
  pure v

private def parseConverses : Lean.Parsec (Theorem → Theorem) := attempt do
  skipString "converse:"
  ws
  let xs ← many $ parseConverse
  pure $ λ p => { p with converses := xs.toList }

private def parsePropertyId : Lean.Parsec String := do
  skipChar 'P'
  let d ← manyChars digit
  skipChar ':'
  ws
  pure ("P" ++ d)

private def parsePropertyExpr : Lean.Parsec Lean.Expr := do
  let uid ← parsePropertyId
  let expr := Lean.Expr.const uid []
  let trueOrFalse ← (pstring "true") <|> (pstring "false")
  ws

  pure $ match trueOrFalse with 
  | "true" => expr
  | _ => Lean.Expr.app (Lean.Expr.const `Not []) expr

private def parseAnd : Lean.Parsec (List Lean.Expr) := attempt do
  ws
  skipString "and:"
  ws
  let xs ← many1 (do
    ws
    skipChar '-'
    ws
    pure $ ← parsePropertyExpr)
  pure xs.toList

private def parseHypothesis : Lean.Parsec (Theorem → Theorem) := attempt do
  skipString "if:"
  ws
  let h ← parseAnd <|> do 
    pure [←parsePropertyExpr]
  pure $ λ p => { p with hypothesis := h }

private def parseConclusion : Lean.Parsec (Theorem → Theorem) := attempt do
  skipString "then:"
  ws
  let expr ← parsePropertyExpr
  pure $ λ p => { p with conclusion := expr }

private def parseUid : Lean.Parsec (Theorem → Theorem) := attempt do
  skipString "uid:"
  ws
  skipChar 'T'
  let d ← manyChars digit
  ws

  pure $ match d.toNat? with
  | none => λ p => p
  | some i => λ p => { p with uid := i }

private def theoremParser : Lean.Parsec Theorem := do
  Lean.Parsec.skipString "---\n"
  let fs ← many (parseHypothesis <|> parseUid <|> parseReferences' <|> parseConverses <|> parseConclusion <|> parseText <|> parseMathlib)
  pure $ fs.foldl (fun x f => f x) { uid := 0 : Theorem }

private def removeComments (s : String) : String :=
  let lines := s.splitOn "\n"
  let filteredLines := lines.filter (fun line => ¬ line.startsWith "#") 
  String.intercalate "\n" filteredLines

private def readFileContents (path : System.FilePath) : IO (Except String Theorem) := do
  IO.FS.withFile path IO.FS.Mode.read fun handle => do
    let s ← handle.readToEnd
    let result := Lean.Parsec.run theoremParser (removeComments s)
    pure result

private def paddedUid (t : Theorem) : String :=
  let str := toString t.uid
  let paddingNeeded := max 0 (6 - str.length)
  let padding := "".pushn '0' paddingNeeded
  "T" ++ padding ++ str

unsafe def readTheorems (path : System.FilePath) : IO (Std.HashMap String Theorem) := do
  let entries <- path.readDir
  let mut result := Std.mkHashMap entries.size

  for entry in entries do
    match (← readFileContents $ path.join entry.fileName) with
    | Except.error e => reportError entry.fileName e
    | Except.ok p =>
      if (paddedUid p) ++ ".md" != entry.fileName then
        reportError entry.fileName s!"uid {paddedUid p} does not agree with filename {entry.fileName}"
      else
        result := result.insert (paddedUid p) p
        IO.println (repr p)
        if p.text == "" then
          reportError entry.fileName "no text"

  pure result

end PiBase

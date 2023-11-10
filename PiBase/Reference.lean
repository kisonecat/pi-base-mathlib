import Lean.Data.Parsec

namespace PiBase

structure Reference where
  citation : String
  kind : String
  name : String
deriving Repr

open Lean.Parsec

def parseKeyValue : Lean.Parsec (Prod String String) := attempt do
  ws
  let k ← manyChars $ satisfy (· != ':')
  skipChar ':'
  ws
  let v ← manyChars $ satisfy (· != '\n')
  skipChar '\n'
  pure ⟨ k, v ⟩

def parseReference : Lean.Parsec Reference := attempt do
  ws
  skipChar '-'
  ws
  let ⟨ k1, v1 ⟩ ← parseKeyValue
  let ⟨ k2, v2 ⟩ ← parseKeyValue
  if k1 != "name" ∧ k2 != "name" then fail "expect name"
  pure $ if k1 == "name"
    then { citation := v2, kind := k2, name := v1 }
    else { citation := v1, kind := k1, name := v2 }

def parseReferences : Lean.Parsec (List Reference) := attempt do
  skipString "refs:"
  ws
  let xs ← many $ parseReference
  pure $ xs.toList

end PiBase

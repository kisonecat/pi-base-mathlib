import Mathlib
import Lean.Data.Parsec
import Lean.Expr
import Lean.Meta

import PiBase.Property
import PiBase.Theorem
import Std

namespace PiBase

unsafe def verifyTheorem (t : Theorem) (env : Std.HashMap String Property ) : IO (Except String Unit) := do
  IO.println (repr t)
  pure $ Except.ok ()

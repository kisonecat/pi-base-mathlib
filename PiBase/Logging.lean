namespace PiBase

def reportError (path : System.FilePath) (e : String) : IO Unit := do
  IO.println s!"error in {path}: {e}"

def reportWarning (path : System.FilePath) (e : String) : IO Unit := do
  IO.println s!"warning in {path}: {e}"

end PiBase


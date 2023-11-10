import Lake
open Lake DSL

package «pi-base-mathlib»

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "nightly"

require CMark from git
  "https://github.com/xubaiw/CMark.lean" @ "main"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

lean_lib PiBase

@[default_target]
lean_exe «pibase» {
  root := `Main
  supportInterpreter := true
}

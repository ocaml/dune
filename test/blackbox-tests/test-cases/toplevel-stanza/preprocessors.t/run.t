Running toplevel with preprocessor
  $ dune exec ./tt.exe -- -init init.ml | sed -E 's/OCaml version .*$/Ocaml version REDACTED/g'
  Ocaml version REDACTED
  Enter #help;; for help.
  
  PPX extension: 42

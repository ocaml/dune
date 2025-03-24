Simple example to run toplevel

  $ dune exec ./tt.exe -- -init init.ml | sed -E 's/OCaml version .*$/OCaml version REDACTED/g'
  OCaml version REDACTED
  Enter #help;; for help.
  
  Foo.x = 42


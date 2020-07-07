Simple example to run toplevel
  $ dune exec --root simple ./tt.exe -- -init simple/init.ml | sed -E 's/OCaml version .*$/OCaml version REDACTED/g'
  Entering directory 'simple'
  Entering directory 'simple'
          OCaml version REDACTED
  
  Foo.x = 42


Running toplevel with preprocessor
  $ dune exec --root preprocessors ./tt.exe -- -init preprocessors/init.ml | sed -E 's/OCaml version .*$/Ocaml version REDACTED/g'
  Entering directory 'preprocessors'
  Entering directory 'preprocessors'
          Ocaml version REDACTED
  
  PPX extension: 42

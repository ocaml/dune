Simple example to run toplevel
  $ dune exec --root simple ./tt.exe -- -init simple/init.ml | sed -E 's/[0-9]+\.[0-9]+\.[0-9]+/REDACTED/g'
  Entering directory 'simple'
  Entering directory 'simple'
          OCaml version REDACTED
  
  Foo.x = 42

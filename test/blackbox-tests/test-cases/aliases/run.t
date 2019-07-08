  $ dune clean --display short
  $ dune build --display short @just-in-src
  running in .
  $ dune clean --display short
  $ dune build --display short @everywhere
  running in bar
  running in baz
  running in .
  $ dune clean --display short
  $ dune build --display short @x
  running in bar
  running in baz
  running in .
  $ dune build --display short @plop
  Error: Alias "plop" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
  $ dune build --display short @truc/x
  Error: Don't know about directory truc specified on the command line!
  [1]

  $ dune clean --display short
  $ dune build --display short @just-in-src
  running in src
  $ dune clean --display short
  $ dune build --display short @everywhere
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ dune clean --display short
  $ dune build --display short @x
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ dune build --display short @plop
  From the command line:
  Error: Alias plop is empty.
  It is not defined in . or any of its descendants.
  [1]
  $ dune build --display short @truc/x
  From the command line:
  Error: Don't know about directory truc!
  [1]

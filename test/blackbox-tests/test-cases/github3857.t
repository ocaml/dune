dune install should not write anything to _build/
  $ echo "(lang dune 2.8)" > dune-project
  $ dune install --prefix _install
  $ ls .
  dune-project

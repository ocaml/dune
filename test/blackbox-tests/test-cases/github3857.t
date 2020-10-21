dune install should not write anything to _build/
  $ echo "(lang dune 2.8)" > dune-project
  $ dune install --prefix _install
  $ ls -aR _build
  ls: _build: No such file or directory
  [1]

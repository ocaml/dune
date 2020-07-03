  $ dune build exec ./aa.exe
  Error: Don't know how to build exec
  [1]
  $ dune build exec ./bb.exe
  Error: Don't know how to build exec
  [1]

  $ mkdir err
  $ echo "(lang dune 2.1)" > err/dune-project
  $ touch err/foo.ml err/stubs.c
  $ cat > err/dune << EOF
  > (executable
  >  (name foo)
  >  (modes exe byte)
  >  (foreign_stubs (language c) (names stubs)))
  > EOF
  $ dune build --root err @all
  Entering directory 'err'
  File "dune", line 1, characters 0-86:
  1 | (executable
  2 |  (name foo)
  3 |  (modes exe byte)
  4 |  (foreign_stubs (language c) (names stubs)))
  Error: Pure bytecode executables cannot contain foreign stubs.
  Hint: If you only need to build a native executable use "(modes exe)".
  [1]

Test optional executable
========================

  $ cat >dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries does-not-exist)
  >  (optional))
  > 
  > (rule
  >  (alias run-x)
  >  (action (run %{exe:x.exe})))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name x))
  > EOF

  $ touch x.ml

  $ dune build @install

  $ dune build @all
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  [1]

  $ dune build @run-x
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  [1]

Reproduction case for a bug in dune < 2.4 where all executables where
considered as optional:

  $ cat >dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries does-not-exist))
  > EOF

The following command should fail because the executable is not optional:

  $ dune build @install
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist))
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  [1]

A strange behavior discovered in #4786. Dune would ignore an executable if any
of its dependencies were optional.

  $ mkdir optional-binary
  $ cd optional-binary
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (package (name myfoo))
  > EOF

  $ mkdir exe
  $ cat >exe/bar.ml <<EOF
  > print_endline "hello world"
  > EOF
  $ cat >exe/dune <<EOF
  > (executable (public_name dunetestbar) (name bar) (libraries foo))
  > EOF

  $ mkdir lib
  $ cat >lib/dune <<EOF
  > (library (name foo) (libraries xxx-does-not-exist) (optional) (modules ()))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias run-x)
  >  (action (echo %{exe:bar.exe})))
  > EOF

  $ dune build @run-x
  Error: No rule found for bar.exe
  -> required by %{exe:bar.exe} at dune:3
  -> required by alias run-x in dune:1
  [1]

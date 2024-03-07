When (include_subdirs qualified) is enabled, we should forbid the same module
to be defined by both a normal compilation unit and a directory of modules

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > module X = Mod
  > EOF
  $ touch mod.ml
  $ mkdir mod
  $ touch mod/baz.ml

  $ dune build foo.exe
  File "dune", line 1, characters 0-0:
  Error: The following module and module group cannot co-exist in the same
  executable or library because they correspond to the same module path
  - module mod.ml
  - module group mod/
  [1]

Another type of overlap:

  $ rm mod/baz.ml
  $ touch mod/mod.ml

  $ dune build foo.exe
  File "dune", line 1, characters 0-0:
  Error: The following module and module group cannot co-exist in the same
  executable or library because they correspond to the same module path
  - module mod.ml
  - module group mod/
  [1]

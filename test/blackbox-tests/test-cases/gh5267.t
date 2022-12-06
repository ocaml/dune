This issue demonstrates a bug when there's a library without any modules and an
executable with a module with the same name as the library's entry point in the
same directory.

  $ cat >dune-project <<EOF
  > (lang dune 1.2)
  > EOF
  $ cat >dune<<EOF
  > (library
  >  (name foo)
  >  (modules))
  > (executable
  >  (name bar)
  >  (libraries foo))
  > EOF
  $ cat >bar.ml <<EOF
  > module M = Foo
  > EOF
  $ touch foo.ml
  $ touch foo.mli

  $ dune build ./bar.exe
  File "dune", line 1, characters 0-0:
  Error: Module "Foo" is used in several stanzas:
  - dune:1
  - dune:4
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

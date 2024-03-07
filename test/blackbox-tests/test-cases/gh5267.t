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
  $ touch foo.ml foo.mli

  $ dune build ./bar.exe
  File "foo.ml-gen", line 1:
  Error: Could not find the .cmi file for interface foo.mli.
  [1]

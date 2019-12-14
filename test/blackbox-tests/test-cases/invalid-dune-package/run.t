This test demonstrates the handling of invalid dune-package files
  $ mkdir -p findlib/baz
  $ touch findlib/baz/dune-package
  $ echo "(lang dune 2.0)" > dune-project
  $ echo 'let () = print_endline "foo"' > foo.ml
  $ cat >dune <<EOF
  > (executable (name foo))
  > (library (name bar) (libraries baz) (modules))
  > EOF
  $ OCAMLPATH=$PWD/findlib dune exec ./foo.exe
  foo

Now we attempt to use an invalid dune-package library:
  $ cat >dune <<EOF
  > (executable (name foo) (libraries baz))
  > EOF
  $ OCAMLPATH=$PWD/findlib dune exec ./foo.exe
  File "dune", line 1, characters 34-37:
  1 | (executable (name foo) (libraries baz))
                                        ^^^
  Error: Library "baz" not found.
  Hint: try: dune external-lib-deps --missing ./foo.exe
  [1]

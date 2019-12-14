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

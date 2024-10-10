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
  File "$TESTCASE_ROOT/findlib/baz/dune-package", line 1, characters 0-0:
  Error: Invalid first line, expected: (lang <lang> <version>)
  -> required by _build/default/.foo.eobjs/byte/dune__exe__Foo.cmi
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  [1]

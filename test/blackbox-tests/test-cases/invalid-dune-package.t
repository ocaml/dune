This test demonstrates the handling of invalid dune-package files
  $ mkdir -p findlib/baz
  $ touch findlib/baz/dune-package
  $ make_dune_project 2.0
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
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  [1]

Now we attempt to use a dune-package file produced by a future version of Dune:
  $ cat >findlib/baz/dune-package <<EOF
  > (lang dune 99.0)
  > EOF
  $ OCAMLPATH=$PWD/findlib dune exec ./foo.exe
  File "$TESTCASE_ROOT/findlib/baz/dune-package", line 1, characters 11-15:
  1 | (lang dune 99.0)
                 ^^^^
  Error: The installed dune package was generated with dune language version
  99.0, which this version of dune cannot read.
  This version of Dune supports dune-package files up to version 3.25.
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  Hint: Upgrade your version of Dune.
  [1]

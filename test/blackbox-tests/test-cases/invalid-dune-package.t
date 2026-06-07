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
  Error: Version 99.0 of the dune language is not supported by this version of
  Dune.
  This version of Dune supports the following versions of the dune language:
  - 1.0 to 1.12
  - 2.0 to 2.9
  - 3.0 to 3.25
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  Hint: Upgrade Dune to a version that supports (lang dune 99.0).
  Hint: If this file is part of your project, you can instead lower the dune
  language version.
  [1]

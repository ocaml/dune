Generated modules must not silently replace other sources in an
(include_subdirs unqualified) group.

  $ make_dune_project 3.24
  $ mkdir a b
  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name collision)
  >  (modules Foo))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries collision))
  > EOF
  $ cat >a/foo.ml <<EOF
  > let value = "handwritten"
  > EOF
  $ cat >b/dune <<EOF
  > (ocamllex foo)
  > EOF
  $ cat >b/foo.mll <<EOF
  > { let value = "generated" }
  > rule token = parse
  > | eof { () }
  > EOF
  $ cat >main.ml <<EOF
  > let () = print_endline Collision.Foo.value
  > EOF
  $ dune exec ./main.exe
  Error: Too many files for module Foo in b:
  - _build/default/a/foo.ml
  - _build/default/b/foo.ml
  [1]

A handwritten interface can accompany a generated implementation.

  $ mv a/foo.ml a/foo.handwritten.ml
  $ cat >a/foo.mli <<EOF
  > val value : string
  > EOF
  $ dune exec ./main.exe
  generated

Two generators in different directories cannot produce the same module.

  $ cat >a/dune <<EOF
  > (ocamllex foo)
  > EOF
  $ cat >a/foo.mll <<EOF
  > { let value = "first generator" }
  > rule token = parse
  > | eof { () }
  > EOF
  $ dune exec ./main.exe
  File "b/dune", line 1, characters 0-14:
  1 | (ocamllex foo)
      ^^^^^^^^^^^^^^
  Error: Too many files for module Foo in b:
  - _build/default/a/foo.ml
  - _build/default/b/foo.ml
  [1]

A selected implementation must not replace a handwritten one either.

  $ mkdir -p selected/a
  $ cat >selected/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat >selected/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name collision)
  >  (modules Foo)
  >  (libraries
  >   (select foo.ml from
  >    (-> foo.fallback.ml))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries collision))
  > EOF
  $ cat >selected/a/foo.ml <<EOF
  > let value = "handwritten"
  > EOF
  $ cat >selected/foo.fallback.ml <<EOF
  > let value = "selected"
  > EOF
  $ cat >selected/main.ml <<EOF
  > let () = print_endline Collision.Foo.value
  > EOF
  $ dune exec --root=selected ./main.exe
  Entering directory 'selected'
  Error: Too many files for module Foo in .:
  - _build/default/a/foo.ml
  - _build/default/foo.ml
  Leaving directory 'selected'
  [1]

A selected interface can accompany a handwritten implementation.

  $ cat >selected/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name collision)
  >  (modules Foo)
  >  (libraries
  >   (select foo.mli from
  >    (-> foo.fallback.mli))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries collision))
  > EOF
  $ cat >selected/foo.fallback.mli <<EOF
  > val value : string
  > EOF
  $ dune exec --root=selected ./main.exe
  handwritten

But a selected interface must not replace a handwritten interface.

  $ cat >selected/a/foo.mli <<EOF
  > val value : string
  > EOF
  $ dune exec --root=selected ./main.exe
  Entering directory 'selected'
  Error: Too many files for module Foo in .:
  - _build/default/a/foo.mli
  - _build/default/foo.mli
  Leaving directory 'selected'
  [1]

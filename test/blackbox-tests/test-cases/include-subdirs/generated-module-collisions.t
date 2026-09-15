Generated modules must not silently replace other sources in an
(include_subdirs unqualified) group. Currently, a generated implementation
replaces a handwritten implementation with the same module name.

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
  generated

A handwritten interface can accompany a generated implementation.

  $ mv a/foo.ml a/foo.handwritten.ml
  $ cat >a/foo.mli <<EOF
  > val value : string
  > EOF
  $ dune exec ./main.exe
  generated

Two generators in different directories also produce the same module.
Currently the second generator replaces the first one.

  $ cat >a/dune <<EOF
  > (ocamllex foo)
  > EOF
  $ cat >a/foo.mll <<EOF
  > { let value = "first generator" }
  > rule token = parse
  > | eof { () }
  > EOF
  $ dune exec ./main.exe
  generated

A selected implementation likewise silently replaces a handwritten one.

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
  selected

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

But a selected interface currently replaces a handwritten interface too,
instead of reporting the conflicting files.

  $ cat >selected/a/foo.mli <<EOF
  > val value : string
  > EOF
  $ dune exec --root=selected ./main.exe
  handwritten

Private libraries using the same library name, in the same context, defined in
different folders.

  $ mkdir -p a b

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > a/dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo))
  > EOF

Without any consumers of the libraries (both are built in separate folders)

  $ dune build --display short
        ocamlc a/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc b/.foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt a/.foo.objs/native/foo.{cmx,o}
        ocamlc a/foo.cma
      ocamlopt b/.foo.objs/native/foo.{cmx,o}
        ocamlc b/foo.cma
      ocamlopt a/foo.{a,cmxa}
      ocamlopt b/foo.{a,cmxa}
      ocamlopt a/foo.cmxs
      ocamlopt b/foo.cmxs

With some consumer of the library

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  File "b/dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: Library with name "foo" is defined in two folders (_build/default/a
  and _build/default/b). Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  [1]

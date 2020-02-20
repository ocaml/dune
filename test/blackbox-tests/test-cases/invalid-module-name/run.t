Dune does not report an invalid module name as an error
  $ echo "(lang dune 2.2)" > dune-project
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF
  $ touch foo.ml foo-as-bar.ml
  $ dune build @all --display short
  File "_build/default", line 1, characters 0-0:
  Warning: The following source file corresponds to an invalid module name:
  - foo-as-bar.ml
  This module is ignored by dune. If it's used to generate a module source,
  consider picking a different extension.
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc foo.cma
      ocamlopt .foo.objs/native/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs

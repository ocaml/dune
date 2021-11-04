@all builds private exe's

  $ dune build --display short --root private-exe @all
  Entering directory 'private-exe'
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
        ocamlc foo.bc
        ocamlc foo.bc-for-jsoo
      ocamlopt foo.exe

@all builds private libs

  $ dune build --display short --root private-lib @all
  Entering directory 'private-lib'
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .bar.objs/native/bar.{cmx,o}
        ocamlc bar.cma
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs

@all builds custom install stanzas

  $ dune build --root install-stanza @subdir/all
  Entering directory 'install-stanza'
  Error: No rule found for subdir/foobar
  -> required by alias subdir/all
  [1]

@all builds user defined rules

  $ dune build --display short --root user-defined @all
  Entering directory 'user-defined'
          echo foo

@all includes user defined install alias

  $ dune build --display short --root install-alias @all
  Entering directory 'install-alias'
          echo foo

@all does not depend directly on file copies from the source tree

  $ mkdir -p source-file-copies
  $ cd source-file-copies
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Add two files

  $ touch a.ml b.ml

An empty project, should not copy any file.

  $ dune build
  $ find _build/default -name '*.ml'

A project that only uses a.ml, should not copy b.ml

  $ cat > dune <<EOF
  > (library (name a) (modules a))
  > EOF
  $ dune build
  $ find _build/default -name '*.ml'
  _build/default/a.ml

A project that uses both files, should copy both.

  $ cat > dune <<EOF
  > (library (name a))
  > EOF
  $ dune build
  $ find _build/default -name '*.ml' | sort
  _build/default/a.ml
  _build/default/b.ml

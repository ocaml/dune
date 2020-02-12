  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (package (name bundledlib))
  > (package
  >  (name bin)
  >  (depends (bundledlib :bundle)))
  > EOF
  $ mkdir bundledlib bin
  $ touch bundledlib/bundledlib.ml bin/bin.ml
  $ cat >bundledlib/dune <<EOF
  > (library (public_name bundledlib))
  > EOF
  $ cat >bin/dune <<EOF
  > (executable (package bin) (public_name bin) (libraries bundledlib))
  > EOF
  $ dune build @install --only-packages bin --display short
      ocamldep bundledlib/.bundledlib.objs/bundledlib.ml.d
        ocamlc bundledlib/.bundledlib.objs/byte/bundledlib.{cmi,cmo,cmt}
        ocamlc bundledlib/bundledlib.cma
      ocamldep bin/.bin.eobjs/bin.ml.d
        ocamlc bin/.bin.eobjs/byte/dune__exe__Bin.{cmi,cmo,cmt}
      ocamlopt bin/.bin.eobjs/native/dune__exe__Bin.{cmx,o}
      ocamlopt bundledlib/.bundledlib.objs/native/bundledlib.{cmx,o}
      ocamlopt bundledlib/bundledlib.{a,cmxa}
      ocamlopt bundledlib/bundledlib.cmxs
      ocamlopt bin/bin.exe

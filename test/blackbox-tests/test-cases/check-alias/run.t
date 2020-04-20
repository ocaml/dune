  $ dune build --root exe @check --display short && ls exe/_build/default/.merlin-conf
  Entering directory 'exe'
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
  exe/_build/default/.merlin-conf

  $ dune build --root lib @check --display short && ls lib/_build/default/.merlin-conf
  Entering directory 'lib'
        ocamlc bar$ext_obj
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
  lib/_build/default/.merlin-conf

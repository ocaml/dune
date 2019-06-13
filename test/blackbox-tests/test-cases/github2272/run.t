The @all alias should only build enabled libraries
  $ dune build @all --display short
      ocamldep disabled/.foo.objs/foo.ml.d
        ocamlc disabled/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc disabled/foo.cma
      ocamlopt disabled/.foo.objs/native/foo.{cmx,o}
      ocamlopt disabled/foo.{a,cmxa}
      ocamlopt disabled/foo.cmxs

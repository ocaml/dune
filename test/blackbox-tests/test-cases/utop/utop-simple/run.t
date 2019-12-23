  $ dune utop --display short forutop -- init_forutop.ml
      ocamldep forutop/.forutop.objs/forutop.ml.d
        ocamlc forutop/.forutop.objs/byte/forutop.{cmi,cmo,cmt}
        ocamlc forutop/forutop.cma
      ocamldep forutop/.utop/.utop.eobjs/utop.ml-gen.d
        ocamlc forutop/.utop/.utop.eobjs/byte/dune__exe__Utop.{cmi,cmo,cmt}
        ocamlc forutop/.utop/utop.exe
  hello in utop

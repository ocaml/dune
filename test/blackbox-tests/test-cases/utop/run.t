  $ $JBUILDER utop -j1 --root . forutop -- init_forutop.ml
      ocamldep forutop/.utop/utop.ml.d
      ocamldep forutop/forutop.ml.d
        ocamlc forutop/forutop.{cmi,cmo,cmt}
        ocamlc forutop/.utop/utop.{cmi,cmo,cmt}
        ocamlc forutop/forutop.cma
        ocamlc forutop/.utop/utop.exe
  hello in utop

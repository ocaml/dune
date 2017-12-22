  $ $JBUILDER utop -j1 --root . forutop -- init_forutop.ml
      ocamldep forutop/.utop/utop.depends.ocamldep-output
      ocamldep forutop/forutop.depends.ocamldep-output
        ocamlc forutop/forutop.{cmi,cmo,cmt}
        ocamlc forutop/.utop/utop.{cmi,cmo,cmt}
        ocamlc forutop/forutop.cma
        ocamlc forutop/.utop/utop.exe
  hello in utop

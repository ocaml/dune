  $ $JBUILDER utop -j1 --display short --root . forutop -- init_forutop.ml
      ocamldep forutop/_utop.ml.d
      ocamldep forutop/forutop.ml.d
        ocamlc forutop/.forutop.objs/forutop.{cmi,cmo,cmt}
        ocamlc forutop/._utop.eobjs/_utop.{cmi,cmo,cmt}
        ocamlc forutop/forutop.cma
        ocamlc forutop/_utop.exe
  hello in utop

  $ jbuilder build @install --display short
      ocamldep a1/a.ml.d
        ocamlc a1/.a.objs/a.{cmi,cmo,cmt}
      ocamlopt a1/.a.objs/a.{cmx,$ext_obj}
      ocamlopt a1/a.{$ext_lib,cmxa}
      ocamlopt a1/a.cmxs
      ocamldep a2/a.ml.d
        ocamlc a2/.a.objs/a.{cmi,cmo,cmt}
      ocamlopt a2/.a.objs/a.{cmx,$ext_obj}
      ocamlopt a2/a.{$ext_lib,cmxa}
      ocamlopt a2/a.cmxs
        ocamlc a1/a.cma
        ocamlc a2/a.cma

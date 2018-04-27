  $ jbuilder runtest --display short
      ocamldep bar.ml.d
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc .bar.objs/bar.{cmi,cmo,cmt}
        ocamlc bar.cma
      ocamlopt .foo.objs/foo.{cmx,$ext_obj}
      ocamlopt .bar.objs/bar.{cmx,$ext_obj}
      ocamlopt bar.{$ext_lib,cmxa}
      ocamlopt bar.cmxs
      ocamlopt foo.{$ext_lib,cmxa}
      ocamlopt foo.cmxs
        ocamlc foo.cma
     ocamlfind test.exe
          test alias runtest
  42 42

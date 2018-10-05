  $ dune build --display=short ./main.exe
      ocamldep .main.eobjs/main.ml.d
      ocamldep .bar.objs/bar.ml.d
        ocamlc .bar.objs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.objs/bar.{cmx,o}
      ocamlopt bar.{a,cmxa}
           gcc foo$ext_obj
            ar libfoo_stubs$ext_lib
        ocamlc .main.eobjs/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/main.{cmx,o}
      ocamlopt main.exe

@all builds private exe's

  $ dune build @all --display short
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
        ocamlc foo.bc
        ocamlc foo.bc-for-jsoo
      ocamlopt foo.exe

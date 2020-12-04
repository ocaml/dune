  $ dune build --display short --debug-dependency-path @all
        ocamlc lib/.ocaml_lib.objs/byte/ocaml_lib.{cmi,cmo,cmt}
      ocamldep lib/.ocaml_lib.objs/a.mli.d
      ocamldep lib/.ocaml_lib.objs/a.ml.d
        coqdep theories/b.v.d
      ocamlopt lib/.ocaml_lib.objs/native/ocaml_lib.{cmx,o}
        ocamlc lib/.ocaml_lib.objs/byte/ocaml_lib__A.{cmi,cmti}
          coqc theories/.b.aux,theories/b.{glob,vo}
      ocamlopt lib/.ocaml_lib.objs/native/ocaml_lib__A.{cmx,o}
        ocamlc lib/.ocaml_lib.objs/byte/ocaml_lib__A.{cmo,cmt}
      ocamlopt lib/ocaml_lib.{a,cmxa}
        ocamlc lib/ocaml_lib.cma
      ocamlopt lib/ocaml_lib.cmxs

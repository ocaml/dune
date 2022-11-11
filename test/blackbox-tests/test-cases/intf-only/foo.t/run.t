Successes:

  $ dune build --display short --debug-dep
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo__.{cmi,cmo,cmt}
      ocamldep .foo.objs/intf.mli.d
      ocamlopt .foo.objs/native/foo__.{cmx,o}
        ocamlc .foo.objs/byte/foo__Intf.{cmi,cmti}
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc test/.bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc foo.cma
      ocamlopt test/.bar.objs/native/bar.{cmx,o}
        ocamlc test/bar.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt test/bar.{a,cmxa}
      ocamlopt foo.cmxs
      ocamlopt test/bar.cmxs

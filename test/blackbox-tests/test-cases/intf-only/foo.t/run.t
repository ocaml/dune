Successes:

  $ dune build --display short --debug-dep
      ocamldep test/.bar.objs/bar.impl.d
      ocamldep .foo.objs/foo.impl.d
        ocamlc .foo.objs/byte/foo__.{cmi,cmo,cmt}
      ocamldep .foo.objs/foo__Intf.intf.d
      ocamlopt .foo.objs/native/foo__.{cmx,o}
        ocamlc .foo.objs/byte/foo__Intf.{cmi,cmti}
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc test/.bar.objs/byte/bar.{cmi,cmo,cmt}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt test/.bar.objs/native/bar.{cmx,o}
        ocamlc test/bar.cma
      ocamlopt foo.cmxs
      ocamlopt test/bar.{a,cmxa}
      ocamlopt test/bar.cmxs

  $ dune build @install @runtest --display short
      ocamldep bin/.main.eobjs/main.ml.d
      ocamldep lib/.hello_world.objs/hello_world.ml.d
        ocamlc lib/.hello_world.objs/hello_world.{cmi,cmo,cmt}
        ocamlc lib/hello_world.cma
      ocamldep test/.test.eobjs/test.ml.d
      ocamlopt lib/.hello_world.objs/hello_world.{cmx,o}
      ocamlopt lib/hello_world.{a,cmxa}
        ocamlc test/.test.eobjs/test.{cmi,cmo,cmt}
      ocamlopt test/.test.eobjs/test.{cmx,o}
      ocamlopt test/test.exe
          test test/test.output
      ocamlopt lib/hello_world.cmxs
        ocamlc bin/.main.eobjs/main.{cmi,cmo,cmt}
      ocamlopt bin/.main.eobjs/main.{cmx,o}
      ocamlopt bin/main.exe

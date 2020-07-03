  $ dune build @install @runtest --display short
      ocamldep lib/.hello_world.objs/hello_world.ml.d
      ocamldep bin/.main.eobjs/main.ml.d
      ocamldep test/.test.eobjs/test.ml.d
        ocamlc lib/.hello_world.objs/byte/hello_world.{cmi,cmo,cmt}
      ocamlopt lib/.hello_world.objs/native/hello_world.{cmx,o}
        ocamlc lib/hello_world.cma
      ocamlopt lib/hello_world.{a,cmxa}
        ocamlc bin/.main.eobjs/byte/main.{cmi,cmo,cmt}
      ocamlopt lib/hello_world.cmxs
        ocamlc test/.test.eobjs/byte/test.{cmi,cmo,cmt}
      ocamlopt bin/.main.eobjs/native/main.{cmx,o}
      ocamlopt test/.test.eobjs/native/test.{cmx,o}
      ocamlopt bin/main.exe
      ocamlopt test/test.exe
          test test/test.output

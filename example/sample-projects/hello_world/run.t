  $ jbuilder build @install @runtest
      ocamldep bin/main.depends.ocamldep-output
      ocamldep lib/hello_world.depends.ocamldep-output
        ocamlc lib/hello_world.{cmi,cmo,cmt}
      ocamlopt lib/hello_world.{cmx,o}
        ocamlc bin/main.{cmi,cmo,cmt}
        ocamlc lib/hello_world.cma
      ocamlopt lib/hello_world.{a,cmxa}
      ocamlopt bin/main.{cmx,o}
      ocamlopt lib/hello_world.cmxs
      ocamlopt bin/main.exe
   hello_world test/hello_world.output
          diff alias test/runtest

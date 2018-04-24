  $ jbuilder build @install @runtest
         ocaml config.full
      ocamldep src/plop.depends.ocamldep-output
        ocamlc src/config.{cmi,cmo,cmt}
      ocamlopt src/config.{cmx,o}
        ocamlc src/plop.{cmi,cmo,cmt}
      ocamlopt src/plop.{cmx,o}
      ocamlopt src/plop.exe

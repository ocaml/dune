Anonymous projects have explicit_js_mode enabled

  $ dune build --display short @all
  Info: Creating file dune-project with this contents:
  | (lang dune 2.8)
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs

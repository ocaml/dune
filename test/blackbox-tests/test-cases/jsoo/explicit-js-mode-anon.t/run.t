Anonymous projects have explicit_js_mode enabled

  $ echo '(lang dune 2.8)' > dune-project
  $ dune build --display short @all
      ocamldep .foo.objs/foo.impl.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs

Anonymous projects have explicit_js_mode enabled
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ dune build --display short @all
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs

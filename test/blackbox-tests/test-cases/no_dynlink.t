  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name mylib))
  > EOF

  $ touch a.ml

  $ dune build --display short
        ocamlc .mylib.objs/byte/mylib.{cmi,cmo,cmt}
      ocamldep .mylib.objs/mylib__A.impl.d
      ocamlopt .mylib.objs/native/mylib.{cmx,o}
        ocamlc .mylib.objs/byte/mylib__A.{cmi,cmo,cmt}
      ocamlopt .mylib.objs/native/mylib__A.{cmx,o}
        ocamlc mylib.cma
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs

  $ cat >dune <<EOF
  > (library
  >  (name mylib)
  >  (no_dynlink))
  > EOF

  $ dune clean

  $ dune build --display short
        ocamlc .mylib.objs/byte/mylib.{cmi,cmo,cmt}
      ocamldep .mylib.objs/mylib__A.impl.d
      ocamlopt .mylib.objs/native/mylib.{cmx,o}
        ocamlc .mylib.objs/byte/mylib__A.{cmi,cmo,cmt}
      ocamlopt .mylib.objs/native/mylib__A.{cmx,o}
        ocamlc mylib.cma
      ocamlopt mylib.{a,cmxa}

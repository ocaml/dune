  $ $JBUILDER runtest -j1 --root .
      ocamldep bar.ml.d
      ocamldep bar_no_unix.ml.d
      ocamldep bar_unix.ml.d
      ocamldep foo.ml.d
      ocamldep foo_fake.ml.d
      ocamldep foo_no_fake.ml.d
      ocamldep main.ml.d
        ocamlc bar.{cmi,cmo,cmt}
        ocamlc foo.{cmi,cmo,cmt}
      ocamlopt bar.{cmx,o}
      ocamlopt foo.{cmx,o}
        ocamlc main.{cmi,cmo,cmt}
      ocamlopt main.{cmx,o}
      ocamlopt main.exe
          main alias runtest
  bar has unix
  foo has no fake

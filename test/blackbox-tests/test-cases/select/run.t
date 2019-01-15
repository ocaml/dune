  $ dune runtest --display short
      ocamldep .main.eobjs/bar.ml.d
      ocamldep .main.eobjs/bar_no_unix.ml.d
      ocamldep .main.eobjs/bar_unix.ml.d
      ocamldep .main.eobjs/foo.ml.d
      ocamldep .main.eobjs/foo_fake.ml.d
      ocamldep .main.eobjs/foo_no_fake.ml.d
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/bar.{cmx,o}
        ocamlc .main.eobjs/byte/foo.{cmi,cmo,cmt}
        ocamlc .main.eobjs/byte/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt .main.eobjs/native/foo.{cmx,o}
      ocamlopt main.exe
          main alias runtest
  bar has unix
  foo has no fake

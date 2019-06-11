  $ dune runtest --display short
      ocamldep .main.eobjs/bar.ml.d
        ocamlc .main.eobjs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/bar.{cmx,o}
      ocamldep .main.eobjs/bar_no_unix.ml.d
        ocamlc .main.eobjs/byte/bar_no_unix.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/bar_no_unix.{cmx,o}
      ocamldep .main.eobjs/bar_unix.ml.d
        ocamlc .main.eobjs/byte/bar_unix.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/bar_unix.{cmx,o}
      ocamldep .main.eobjs/foo.ml.d
        ocamlc .main.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/foo.{cmx,o}
      ocamldep .main.eobjs/foo_fake.ml.d
        ocamlc .main.eobjs/byte/foo_fake.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/foo_fake.{cmx,o}
      ocamldep .main.eobjs/foo_no_fake.ml.d
        ocamlc .main.eobjs/byte/foo_no_fake.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/foo_no_fake.{cmx,o}
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe
          main alias runtest
  bar has unix
  foo has no fake

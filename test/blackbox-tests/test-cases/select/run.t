  $ echo '(lang dune 1.0)' > dune-project
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries
  >   (select bar.ml from
  >    (unix -> bar.unix.ml)
  >    (!unix -> bar.no_unix.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > (alias
  >  (name runtest)
  >  (action (run ./main.exe)))
  > EOF

  $ dune runtest --display short
      ocamldep .main.eobjs/bar.ml.d
      ocamldep .main.eobjs/foo.ml.d
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

  $ echo '(lang dune 2.0)' > dune-project
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries
  >   (select bar.ml from
  >    (unix -> bar.unix.ml)
  >    (!unix -> bar.no_unix.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > (rule
  >  (alias runtest)
  >  (action (run ./main.exe)))
  > EOF

  $ dune runtest --display short
        ocamlc .main.eobjs/byte/dune__exe.{cmi,cmo,cmt}
        ocamlc .main.eobjs/byte/dune__exe__Bar.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Bar.{cmx,o}
        ocamlc .main.eobjs/byte/dune__exe__Foo.{cmi,cmo,cmt}
        ocamlc .main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamlopt .main.eobjs/native/dune__exe.{cmx,o}
      ocamlopt .main.eobjs/native/dune__exe__Foo.{cmx,o}
      ocamlopt main.exe
          main alias runtest
  bar has unix
  foo has no fake

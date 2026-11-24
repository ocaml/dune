using `(select ...)` in `(library (modes melange) ..)`

  $ make_melange_project 3.22 1.0

  $ cat >bar.melange.ml <<EOF
  > let message = "hello from melange"
  > EOF

  $ touch bar.native.ml foo.fake.ml
  $ cat >foo.no_fake.ml <<EOF
  > let message = "foo has no fake"
  > EOF
  $ cat >lib.ml <<EOF
  > let () = Js.log Bar.message
  > let () = Js.log Foo.message
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes melange)
  >  (libraries
  >   (select bar.ml from
  >    (melange.dom -> bar.melange.ml)
  >    (!melange.dom -> bar.native.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > EOF

  $ dune build
  $ ls _build/default/.lib.objs/melange | grep '\.cmj$'
  lib.cmj
  lib__.cmj
  lib__Bar.cmj
  lib__Foo.cmj

A select declared only in melange.libraries also contributes its result to
the library's modules.

  $ make_melange_project 3.26 1.0
  $ mkdir selected app
  $ cat >selected/dune <<EOF
  > (library
  >  (name picked)
  >  (modes melange)
  >  (melange.libraries
  >   (select choice.ml from
  >    (melange -> choice.present.ml)
  >    (!melange -> choice.absent.ml))))
  > EOF
  $ cat >selected/choice.present.ml <<EOF
  > let value = "selected"
  > EOF
  $ cat >selected/choice.absent.ml <<EOF
  > let value = "absent"
  > EOF
  $ cat >selected/picked.ml <<EOF
  > let value = Choice.value
  > EOF
  $ cat >app/dune <<EOF
  > (melange.emit (target output) (libraries picked))
  > EOF
  $ cat >app/main.ml <<EOF
  > let () = Js.log Picked.value
  > EOF
  $ dune build @app/melange
  $ node _build/default/app/output/app/main.js
  selected
  $ test -f _build/default/selected/.melange_src/choice.ml

Without melange.libraries, Melange inherits selects from libraries.

  $ cat >selected/dune <<EOF
  > (library
  >  (name picked)
  >  (modes melange)
  >  (libraries
  >   (select choice.ml from
  >    (melange -> choice.present.ml)
  >    (!melange -> choice.absent.ml))))
  > EOF
  $ dune build @app/melange
  $ node _build/default/app/output/app/main.js
  selected

The OCaml and Melange modes can select different modules in the same library.

  $ mkdir mixed
  $ cat >mixed/dune <<EOF
  > (library
  >  (name mixed)
  >  (modes byte melange)
  >  (libraries
  >   (select ocaml_only.ml from (-> ocaml_only.available.ml)))
  >  (melange.libraries
  >   (select melange_only.ml from (-> melange_only.available.ml))))
  > EOF
  $ echo 'let value = "ocaml"' >mixed/ocaml_only.available.ml
  $ echo 'let value = "melange"' >mixed/melange_only.available.ml
  $ dune build mixed/mixed.cma
  $ test -f _build/default/mixed/.mixed.objs/byte/mixed__Ocaml_only.cmi
  $ test ! -f _build/default/mixed/.mixed.objs/byte/mixed__Melange_only.cmi
  $ cat >app/dune <<EOF
  > (melange.emit (target output) (libraries mixed))
  > EOF
  $ cat >app/main.ml <<EOF
  > let () = Js.log Mixed.Melange_only.value
  > EOF
  $ dune build @app/melange
  $ node _build/default/app/output/app/main.js
  melange

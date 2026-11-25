using `(select ...)` in melange.emit

  $ make_melange_project 3.8 0.1
  $ cat >bar.melange.ml <<EOF
  > let message = "hello from melange"
  > EOF
  $ cat >bar.native.ml <<EOF
  > let message = print_endline "hello from native"
  > EOF
  $ cat >foo.fake.ml <<EOF
  > let message = "foo has fake " ^^ Fakefoobar.fake
  > EOF
  $ cat >foo.no_fake.ml <<EOF
  > let message = "foo has no fake"
  > EOF
  $ cat >main.ml <<EOF
  > let () = Js.log Bar.message
  > let () = Js.log Foo.message
  > EOF
  $ cat >dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (libraries
  >   (select bar.ml from
  >    (melange.dom -> bar.melange.ml)
  >    (!melange.dom -> bar.native.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > EOF

  $ dune build @mel
  $ node ./_build/default/output/main.js
  hello from melange
  foo has no fake

A neighboring library can exclude the emit stanza's selected modules.

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes melange)
  >  (modules :standard \ main bar foo))
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (modules main bar foo)
  >  (libraries lib
  >   (select bar.ml from
  >    (melange.dom -> bar.melange.ml)
  >    (!melange.dom -> bar.native.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > EOF
  $ dune build @mel

Select targets in qualified subdirectories retain their module paths.

  $ make_melange_project 3.26 1.0
  $ mkdir -p qualified/group
  $ cat >qualified/dune <<EOF
  > (include_subdirs qualified)
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries
  >   (select group/choice.ml from
  >    (melange.dom -> group/choice.available.ml)
  >    (!melange.dom -> group/choice.unavailable.ml))))
  > EOF
  $ cat >qualified/group/choice.available.ml <<EOF
  > let message = "hello from a qualified select"
  > EOF
  $ cat >qualified/group/choice.unavailable.ml <<EOF
  > let message = "dependency unavailable"
  > EOF
  $ cat >qualified/main.ml <<EOF
  > let () = print_endline Group.Choice.message
  > EOF
  $ dune build @qualified/melange
  $ node _build/default/qualified/output/qualified/main.js
  hello from a qualified select

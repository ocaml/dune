using `(select ...)` in melange.emit

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
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
  >    (melange -> bar.melange.ml)
  >    (!melange -> bar.native.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > EOF

  $ dune build @mel
  $ node ./_build/default/output/main.js
  hello from melange
  foo has no fake


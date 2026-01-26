using `(select ...)` in `(library (modes melange) ..)`

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > EOF

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

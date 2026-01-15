Test -p flag with multiple vendor stanzas.

Setup two vendor directories with different packages:

  $ mkdir -p foo bar

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name app))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name app)
  >  (public_name app)
  >  (libraries foo bar))
  > (vendor foo)
  > (vendor bar)
  > EOF

  $ cat >app.ml <<EOF
  > let () =
  >   print_endline ("foo: " ^ Foo.version);
  >   print_endline ("bar: " ^ Bar.version)
  > EOF

  $ cat >foo/dune-project <<EOF
  > (lang dune 3.18)
  > (package (name foo))
  > EOF

  $ cat >foo/dune <<EOF
  > (library (name foo) (public_name foo))
  > EOF

  $ cat >foo/foo.ml <<EOF
  > let version = "1.0.0"
  > EOF

  $ cat >bar/dune-project <<EOF
  > (lang dune 3.18)
  > (package (name bar))
  > EOF

  $ cat >bar/dune <<EOF
  > (library (name bar) (public_name bar))
  > EOF

  $ cat >bar/bar.ml <<EOF
  > let version = "2.0.0"
  > EOF

Build without -p:

  $ dune exec ./app.exe
  foo: 1.0.0
  bar: 2.0.0

Build with -p app (vendored packages should still be available):

  $ dune exec -p app ./app.exe
  foo: 1.0.0
  bar: 2.0.0

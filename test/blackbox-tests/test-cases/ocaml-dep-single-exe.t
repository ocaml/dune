When creating single module executables or libraries, running ocamldep isn't
necessary.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ cat >dune <<EOF
  > (executable (name foo))
  > EOF
  $ cat >foo.ml <<EOF
  > print_endline "hello world"
  > EOF
  $ dune exec ./foo.exe
  hello world

We check to see if ocamldep artifacts have been created:
  $ find _build/default -name "*.all-deps" -or -name "*.d"
  _build/default/.foo.eobjs/dune__exe__Foo.impl.all-deps
  _build/default/.foo.eobjs/foo.ml.d

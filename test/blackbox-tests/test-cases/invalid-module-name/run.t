Dune does not report an invalid module name as an error
  $ echo "(lang dune 2.2)" > dune-project
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF
  $ touch foo.ml foo-as-bar.ml
  $ dune build @all --display short
      ocamldep .foo.objs/foo.ml.d
  File "foo__.ml-gen", line 2, characters 10-11:
  2 | module Foo-as-bar = Foo__Foo-as-bar
                ^
  Error: Syntax error
      ocamldep .foo.objs/foo-as-bar.ml.d
  [1]

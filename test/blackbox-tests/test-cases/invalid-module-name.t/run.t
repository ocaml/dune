Dune does not report an invalid module name as an error
  $ echo "(lang dune 2.2)" > dune-project
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF
  $ touch foo.ml foo-as-bar.ml
  $ dune build @all
  Error: foo__Foo-as-bar corresponds to an invalid module name
  -> required by _build/default/foo__.ml-gen
  -> required by _build/default/.foo.objs/byte/foo__.cmi
  -> required by _build/default/.foo.objs/native/foo__.cmx
  -> required by _build/default/foo.a
  -> required by alias all
  [1]

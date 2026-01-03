Test module path validation for `(select ..)` targets when using
`(include_subdirs qualified)`

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select foo-bar/baz.ml from
  >    (unix -> foo-bar/baz.unix.ml)
  >    (!unix -> foo-bar/baz.nounix.ml))))
  > EOF

  $ mkdir -p foo-bar
  $ cat > foo-bar/baz.unix.ml <<EOF
  > let msg = "unix"
  > EOF
  $ cat > foo-bar/baz.nounix.ml <<EOF
  > let msg = "no unix"
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline Foo_bar.Baz.msg
  > EOF

  $ dune build
  Error: foo__Foo-bar/baz corresponds to an invalid module name
  -> required by _build/default/foo__.ml-gen
  -> required by alias all
  -> required by alias default
  [1]

  $ mkdir -p foo_bar
  $ cat > foo_bar/baz.unix.ml <<EOF
  > let msg = "unix"
  > EOF
  $ cat > foo_bar/baz.nounix.ml <<EOF
  > let msg = "no unix"
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select foo_bar/baz-qux.ml from
  >    (unix -> foo_bar/baz-qux.unix.ml)
  >    (!unix -> foo_bar/baz-qux.nounix.ml))))
  > EOF

  $ dune build --display=short
  Error: foo__Foo_bar/baz-qux corresponds to an invalid module name
  -> required by _build/default/foo__.ml-gen
  -> required by _build/default/.foo.objs/byte/foo__.cmi
  -> required by _build/default/.foo.objs/native/foo.cmx
  -> required by _build/default/foo.a
  -> required by alias all
  -> required by alias default
  File "dune", lines 5-7, characters 2-111:
  5 |   (select foo_bar/baz-qux.ml from
  6 |    (unix -> foo_bar/baz-qux.unix.ml)
  7 |    (!unix -> foo_bar/baz-qux.nounix.ml))))
  Error: No rule found for foo_bar/baz-qux.unix.ml
  [1]

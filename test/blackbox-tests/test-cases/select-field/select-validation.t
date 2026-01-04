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
  File "dune", lines 5-7, characters 2-99:
  5 |   (select foo-bar/baz.ml from
  6 |    (unix -> foo-bar/baz.unix.ml)
  7 |    (!unix -> foo-bar/baz.nounix.ml))))
  Error: "foo-bar" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: foo_bar would be a correct module name
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
  File "dune", lines 5-7, characters 2-111:
  5 |   (select foo_bar/baz-qux.ml from
  6 |    (unix -> foo_bar/baz-qux.unix.ml)
  7 |    (!unix -> foo_bar/baz-qux.nounix.ml))))
  Error: "baz-qux" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: baz_qux would be a correct module name
  [1]

`(select ..)` not included in the modules set, but still validated as a module
name

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (modules foo)
  >  (libraries
  >   (select foo_bar/baz-qux.ml from
  >    (unix -> foo_bar/baz-qux.unix.ml)
  >    (!unix -> foo_bar/baz-qux.nounix.ml))))
  > EOF

  $ dune build
  File "dune", lines 6-8, characters 2-111:
  6 |   (select foo_bar/baz-qux.ml from
  7 |    (unix -> foo_bar/baz-qux.unix.ml)
  8 |    (!unix -> foo_bar/baz-qux.nounix.ml))))
  Error: "baz-qux" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: baz_qux would be a correct module name
  [1]

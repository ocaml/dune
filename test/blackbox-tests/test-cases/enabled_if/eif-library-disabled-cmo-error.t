Test behavior when targeting artifacts of a disabled library

  $ mkdir -p a b
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if false))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build %{cmo:foo}
  File "command line", line 1, characters 0-10:
  Error: Module Foo does not exist.
  [1]


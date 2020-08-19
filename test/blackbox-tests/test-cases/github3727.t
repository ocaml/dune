This bug demonstrate a distinction between public & private names.

When -p was used, private names would dissapear as they would be filtered by the
stanza filter. This behavior is incorrect and private names should remain
visible regardless if the stanzas were filtered.

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name private_foo)
  >  (public_name foo.bar)
  >  (modules private_foo))
  > 
  > (executable
  >  (public_name bin)
  >  (libraries private_foo)
  >  (modules bin)
  >  (package bar))
  > EOF

  $ cat >bin.ml <<EOF
  > print_endline Private_foo.secret
  > EOF

  $ cat >private_foo.ml <<EOF
  > let secret = "private_foo"
  > EOF

  $ dune exec ./bin.exe
  private_foo

  $ dune build -p bar
  File "dune", line 8, characters 12-23:
  8 |  (libraries private_foo)
                  ^^^^^^^^^^^
  Error: Library "private_foo" not found.
  Hint: try:
    dune external-lib-deps --missing -p bar @install
  [1]

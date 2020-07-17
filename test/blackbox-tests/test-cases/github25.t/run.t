This test define an installed "plop" with a "plop.ca-marche-pas"
sub-package which depend on a library that doesn't exist.

The build itself uses only "plop.ca-marche", which doesn't have this
problem. So dune shouldn't crash because of "plop.ca-marche-pas"

We need ocamlfind to run this test

  $ export OCAMLPATH="./findlib-packages"

  $ dune build @install --only hello

  $ dune build @install --only pas-de-bol 2>&1 | sed 's/[^ "]*findlib-packages/.../'
  File ".../plop/META", line 1, characters 0-0:
  Error: Library "une-lib-qui-nexiste-pas" not found.
  -> required by library "plop.ca-marche-pas" in
     .../plop
  Hint: try:
    dune external-lib-deps --missing --only pas-de-bol @install

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
  -> required by _build/default/root/META.pas-de-bol
  -> required by _build/install/default/lib/pas-de-bol/META
  -> required by _build/default/root/pas-de-bol.install
  -> required by alias root/install

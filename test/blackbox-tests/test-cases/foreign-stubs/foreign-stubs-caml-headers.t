Check that C stub compilation rules are made to depend on the Caml header files.

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (names cstub)
  >   (language c)))
  > EOF

  $ touch main.ml cstub.c

Copy a minimal set of Caml header and config files for the test.

  $ mkdir -p _caml/caml
  $ cp $(ocamlc -where)/Makefile.config _caml/
  $ cp $(ocamlc -where)/caml/*.h _caml/caml/
  $ export OCAMLLIB=$(pwd)/_caml

We compile a first time...

  $ dune build --display short _build/default/cstub.o 2>&1 | sed -E 's/ *[^ ]+ *//'
  .dune/cc_vendor/cc_vendor
  cstub.o

And check that we recompile the stub after modifying a Caml header file.

  $ touch _caml/caml/new-header.h
  $ dune build --display short _build/default/cstub.o 2>&1 | sed -E 's/ *[^ ]+ *//'
  cstub.o

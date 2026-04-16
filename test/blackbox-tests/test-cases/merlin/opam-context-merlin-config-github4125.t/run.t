  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ unset OPAMCONFIRMLEVEL

We call `$(opam switch show)` so that this test always uses an existing switch

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context (default))
  > (context
  > (opam
  >  (name cross)
  >  (switch $(opam switch show))
  >  (merlin)))
  > EOF

  $ dune build

  $ ls -a _build/cross/.merlin-conf
  .
  ..
  lib-foo

  $ dune ocaml merlin dump-config --format=json "$PWD" | jq -r '
  >   include "dune";
  >   merlinEntry("Foo")
  >   | merlinJsonEntryWithConfigNames(["STDLIB", "UNIT_NAME"])
  > '
  Foo: _build/cross/foo
  ["STDLIB","/OCAMLC_WHERE"]
  ["UNIT_NAME","foo"]
  Foo: _build/cross/foo.ml
  ["STDLIB","/OCAMLC_WHERE"]
  ["UNIT_NAME","foo"]

Ignores user `.merlin` files when generating Merlin configuration.

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config --format=json $PWD | jq '
  >   include "dune";
  >   merlinEntry("Foo")
  >   | merlinUnitNameSummary'
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo",
    "unit_name": "foo"
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo.ml-gen",
    "unit_name": "foo"
  }

  $ rm -f .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config --format=json $PWD | jq '
  >   include "dune";
  >   merlinEntry("Foo")
  >   | merlinUnitNameSummary'
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo",
    "unit_name": "foo"
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo.ml-gen",
    "unit_name": "foo"
  }

  $ echo toto > .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config --format=json $PWD | jq '
  >   include "dune";
  >   merlinEntry("Foo")
  >   | merlinUnitNameSummary'
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo",
    "unit_name": "foo"
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo.ml-gen",
    "unit_name": "foo"
  }

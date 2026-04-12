  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ ocamlfind_libs="$(ocamlfind printconf path | while read line; do printf lib=${line}:; done)"
  $ export BUILD_PATH_PREFIX_MAP="$ocamlfind_libs:$BUILD_PATH_PREFIX_MAP"

Here we test that instrumentation processing is not passed to merlin by setting
up a project with instrumentation and testing checking the merlin config.

  $ dune build --instrument-with hello ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml merlin dump-config --format=json $PWD/lib \
  >   | jq -r '
  >     include "dune";
  >     .[]
  >     | select(
  >         .module_name == "Bar"
  >         or .module_name == "Foo"
  >         or .module_name == "Privmod"
  >       )
  >     | merlinJsonEntryWithConfigNames(["FLG", "UNIT_NAME"])'
  Bar: _build/default/lib/bar
  ["FLG",["-w","-40","-g"]]
  ["UNIT_NAME","bar"]
  Bar: _build/default/lib/bar.ml-gen
  ["FLG",["-w","-40","-g"]]
  ["UNIT_NAME","bar"]
  Foo: _build/default/lib/foo
  ["FLG",["-w","-40","-g"]]
  ["UNIT_NAME","foo"]
  Foo: _build/default/lib/foo.ml-gen
  ["FLG",["-w","-40","-g"]]
  ["UNIT_NAME","foo"]
  Privmod: _build/default/lib/privmod
  ["FLG",["-w","-40","-g"]]
  ["FLG",["-open","Foo"]]
  ["UNIT_NAME","foo__Privmod"]
  Privmod: _build/default/lib/privmod.ml
  ["FLG",["-w","-40","-g"]]
  ["FLG",["-open","Foo"]]
  ["UNIT_NAME","foo__Privmod"]

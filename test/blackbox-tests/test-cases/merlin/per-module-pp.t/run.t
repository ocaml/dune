  $ dune build @check

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

We dump the config for Foo and Bar modules but the pp.exe preprocessor
should appear only once since only Foo is using it.

  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   .[]
  >   | select(.module_name == "Bar" or .module_name == "Foo")
  >   | merlinJsonEntryWithConfigNames(["FLG", "UNIT_NAME"])
  > '
  Bar: _build/default/bar
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g"]]
  ["UNIT_NAME","bar"]
  Bar: _build/default/bar.ml
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g"]]
  ["UNIT_NAME","bar"]
  Foo: _build/default/foo
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g"]]
  ["FLG",["-pp","$TESTCASE_ROOT/_build/default/pp/pp.exe"]]
  ["UNIT_NAME","foo"]
  Foo: _build/default/foo.ml
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g"]]
  ["FLG",["-pp","$TESTCASE_ROOT/_build/default/pp/pp.exe"]]
  ["UNIT_NAME","foo"]

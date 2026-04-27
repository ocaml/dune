This test demonstrates that -ppx is no more missing when two stanzas are
in the same dune file, but require different ppx specifications

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune build @all --profile release
  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   .[]
  >   | select(.module_name | test("^Usesppx"))
  >   | merlinJsonEntryWithConfigNames(["FLG", "UNIT_NAME"])
  > ' | censor
  Usesppx1: _build/default/usesppx1
  ["FLG",["-w","-40","-g"]]
  ["FLG",["-ppx","$PWD/_build/default/.ppx/$DIGEST1/ppx.exe --as-ppx --cookie 'library-name=\"usesppx1\"'"]]
  ["UNIT_NAME","usesppx1"]
  Usesppx1: _build/default/usesppx1.ml-gen
  ["FLG",["-w","-40","-g"]]
  ["FLG",["-ppx","$PWD/_build/default/.ppx/$DIGEST1/ppx.exe --as-ppx --cookie 'library-name=\"usesppx1\"'"]]
  ["UNIT_NAME","usesppx1"]
  Usesppx2: _build/default/usesppx2
  ["FLG",["-w","-40","-g"]]
  ["FLG",["-ppx","$PWD/_build/default/.ppx/$DIGEST2/ppx.exe --as-ppx --cookie 'library-name=\"usesppx2\"'"]]
  ["UNIT_NAME","usesppx2"]
  Usesppx2: _build/default/usesppx2.ml-gen
  ["FLG",["-w","-40","-g"]]
  ["FLG",["-ppx","$PWD/_build/default/.ppx/$DIGEST2/ppx.exe --as-ppx --cookie 'library-name=\"usesppx2\"'"]]
  ["UNIT_NAME","usesppx2"]

Tests Merlin suffix handling.

  $ dune build @check

  $ dune ocaml merlin dump-config --format=json $PWD | jq -c '
  > include "dune";
  > .[] | merlinConfigItemsNamed(["SUFFIX"])
  > '
  ["SUFFIX",".aml .amli"]
  ["SUFFIX",".baml .bamli"]
  ["SUFFIX",".aml .amli"]
  ["SUFFIX",".baml .bamli"]

  $ cat >alterexe.amli <<EOF
  > (* empty *)
  > EOF

  $ dune build .merlin-conf/exe-alterexe

  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '.[].source_path'
  default/alterexe
  default/alterexe.aml
  default/alterexe.amli

  $ dune ocaml merlin dump-config --format=json $PWD \
  >   | jq -r '.[].source_path'
  default/alterexe
  default/alterexe.aml
  default/alterexe.amli

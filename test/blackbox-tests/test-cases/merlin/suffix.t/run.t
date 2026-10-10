Tests Merlin suffix handling.

  $ dune build @check

  $ dune ocaml merlin dump-config --format=json $PWD | jq_dune -c '
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

Use different readers to distinguish implementation and interface configurations.

  $ rm dune-project
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (executables_implicit_empty_intf false)
  > (dialect
  >  (name altercaml)
  >  (implementation
  >   (extension aml)
  >   (merlin_reader implementation))
  >  (interface
  >   (extension amli)
  >   (merlin_reader interface)))
  > EOF
  $ dune build .merlin-conf/exe-alterexe

The fallback uses the extension to distinguish implementation and interface
configurations, just like exact lookups.

  $ for file in alterexe.aml alterexe.amli alterexe.pp.aml alterexe.pp.amli; do
  >   printf '%s: ' "$file"
  >   query_ocaml_merlin_pp "$file" | grep -Eo '\(READER \([^)]*\)\)'
  > done
  alterexe.aml: (READER (implementation))
  alterexe.amli: (READER (interface))
  alterexe.pp.aml: (READER (implementation))
  alterexe.pp.amli: (READER (interface))

Queries without a matching extension keep the legacy fallback.

  $ for file in alterexe.pp alterexe; do
  >   printf '%s: ' "$file"
  >   query_ocaml_merlin_pp "$file" \
  >     | grep -Eo '\(READER \([^)]*\)\)|\(ERROR "[^"]*"\)'
  > done
  alterexe.pp: (READER (interface))
  alterexe: (READER (interface))

The typed lookup reports mode, default status, source kind, and counterpart.
It omits ambiguous matches instead of assigning them a source kind.

  $ merlin_configurations _build/default/.merlin-conf/exe-alterexe \
  >   alterexe.aml alterexe.amli alterexe.pp.aml alterexe.pp.amli \
  >   alterexe.pp alterexe missing.aml
  alterexe.aml: ocaml true impl alterexe.amli
  alterexe.amli: ocaml true intf alterexe.aml
  alterexe.pp.aml: ocaml true impl alterexe.amli
  alterexe.pp.amli: ocaml true intf alterexe.aml
  alterexe.pp: none
  alterexe: none
  missing.aml: none

The typed lookup keeps each source kind paired with its reader directive.

  $ merlin_configurations --json _build/default/.merlin-conf/exe-alterexe \
  >   alterexe.aml alterexe.amli alterexe.pp.aml alterexe.pp.amli | jq -r '
  > [ .file, .kind,
  >   (.directives[] | select(.[0] == "READER") | .[1][])
  > ] | join(" ")'
  alterexe.aml impl implementation
  alterexe.amli intf interface
  alterexe.pp.aml impl implementation
  alterexe.pp.amli intf interface

The fallback remains available when there is only one candidate.

  $ rm alterexe.amli
  $ dune build .merlin-conf/exe-alterexe
  $ query_ocaml_merlin_pp alterexe.pp | grep -Eo '\(READER \([^)]*\)\)'
  (READER (implementation))

  $ merlin_configurations _build/default/.merlin-conf/exe-alterexe \
  >   alterexe.pp alterexe
  alterexe.pp: ocaml true impl -
  alterexe: ocaml true impl -

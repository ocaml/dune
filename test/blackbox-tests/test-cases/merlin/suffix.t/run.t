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

The fallback remains available when there is only one candidate.

  $ rm alterexe.amli
  $ dune build .merlin-conf/exe-alterexe
  $ query_ocaml_merlin_pp alterexe.pp | grep -Eo '\(READER \([^)]*\)\)'
  (READER (implementation))

The legacy fallback follows filename ordering, even when the implementation
extension sorts after the interface extension.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (executables_implicit_empty_intf false)
  > (dialect
  >  (name altercaml)
  >  (implementation
  >   (extension zml)
  >   (merlin_reader implementation))
  >  (interface
  >   (extension amli)
  >   (merlin_reader interface)))
  > EOF
  $ mv alterexe.aml alterexe.zml
  $ touch alterexe.amli
  $ dune build .merlin-conf/exe-alterexe
  $ for file in alterexe.zml alterexe.amli alterexe.pp.zml alterexe.pp.amli \
  >   alterexe.pp alterexe; do
  >   printf '%s: ' "$file"
  >   query_ocaml_merlin_pp "$file" | grep -Eo '\(READER \([^)]*\)\)'
  > done
  alterexe.zml: (READER (implementation))
  alterexe.amli: (READER (interface))
  alterexe.pp.zml: (READER (implementation))
  alterexe.pp.amli: (READER (interface))
  alterexe.pp: (READER (implementation))
  alterexe: (READER (implementation))

An interface-only module keeps the fallback available.

  $ rm alterexe.zml
  $ rm dune
  $ cat > dune <<EOF
  > (library
  >  (name only)
  >  (modules alterexe)
  >  (modules_without_implementation alterexe))
  > EOF
  $ dune build .merlin-conf/lib-only
  $ for file in alterexe.amli alterexe.pp.amli alterexe.pp alterexe; do
  >   printf '%s: ' "$file"
  >   query_ocaml_merlin_pp "$file" | grep -Eo '\(READER \([^)]*\)\)'
  > done
  alterexe.amli: (READER (interface))
  alterexe.pp.amli: (READER (interface))
  alterexe.pp: (READER (interface))
  alterexe: (READER (interface))

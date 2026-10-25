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
  $ merlin_configurations _build/default/.merlin-conf/exe-alterexe \
  >   alterexe.zml alterexe.amli alterexe.pp.zml alterexe.pp.amli \
  >   alterexe.pp alterexe
  alterexe.zml: ocaml true impl alterexe.amli
  alterexe.amli: ocaml true intf alterexe.zml
  alterexe.pp.zml: ocaml true impl alterexe.amli
  alterexe.pp.amli: ocaml true intf alterexe.zml
  alterexe.pp: none
  alterexe: none

An interface-only module has an unambiguous fallback and no counterpart.

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
  $ merlin_configurations _build/default/.merlin-conf/lib-only \
  >   alterexe.amli alterexe.pp.amli alterexe.pp alterexe
  alterexe.amli: ocaml true intf -
  alterexe.pp.amli: ocaml true intf -
  alterexe.pp: ocaml true intf -
  alterexe: ocaml true intf -

Compound dialect suffixes sharing their final extension lose their readers and
cannot be distinguished by the filename fallback. The dialect preprocessing
also replaces the compilation sources' dialect with OCaml.

  $ mkdir compound
  $ cat > compound/dune-project <<EOF
  > (lang dune 3.16)
  > (dialect
  >  (name compound)
  >  (implementation
  >   (extension impl.ml)
  >   (preprocess (run cat %{input-file}))
  >   (merlin_reader implementation))
  >  (interface
  >   (extension intf.ml)
  >   (preprocess (run cat %{input-file}))
  >   (merlin_reader interface)))
  > EOF
  $ cat > compound/dune <<EOF
  > (library (name sample))
  > EOF
  $ touch compound/sample.impl.ml compound/sample.intf.ml
  $ dune build --root compound @check
  $ (cd compound && merlin_configurations \
  >   _build/default/.merlin-conf/lib-sample \
  >   sample.impl.ml sample.intf.ml sample.pp.impl.ml sample.pp.intf.ml \
  >   sample.unknown sample)
  sample.impl.ml: ocaml true impl sample.intf.ml
  sample.intf.ml: ocaml true intf sample.impl.ml
  sample.pp.impl.ml: none
  sample.pp.intf.ml: none
  sample.unknown: none
  sample: none

  $ for file in sample.impl.ml sample.intf.ml \
  >   sample.pp.impl.ml sample.pp.intf.ml; do
  >   printf '%s: ' "$file"
  >   query_ocaml_merlin_pp "$PWD/compound/$file" --root compound \
  >     | grep -Eo '\(READER \([^)]*\)\)' || echo no-reader
  > done
  sample.impl.ml: no-reader
  sample.intf.ml: no-reader
  sample.pp.impl.ml: no-reader
  sample.pp.intf.ml: no-reader

If preprocessing changes the complete suffix, the final extension can still
identify a unique source kind.

  $ mv compound/sample.intf.ml compound/sample.intf.mli
  $ cat > compound/dune-project <<EOF
  > (lang dune 3.16)
  > (dialect
  >  (name compound)
  >  (implementation
  >   (extension impl.ml)
  >   (preprocess (run cat %{input-file}))
  >   (merlin_reader implementation))
  >  (interface
  >   (extension intf.mli)
  >   (preprocess (run cat %{input-file}))
  >   (merlin_reader interface)))
  > EOF
  $ dune build --root compound @check
  $ (cd compound && merlin_configurations \
  >   _build/default/.merlin-conf/lib-sample \
  >   sample.pp.ml sample.impl.pp.ml sample.intf.pp.mli)
  sample.pp.ml: ocaml true impl sample.intf.mli
  sample.impl.pp.ml: ocaml true impl sample.intf.mli
  sample.intf.pp.mli: ocaml true intf sample.impl.ml

When one declared suffix ends with the other, even an exact interface lookup
uses the implementation reader.

  $ mkdir overlap
  $ cat > overlap/dune-project <<EOF
  > (lang dune 3.16)
  > (dialect
  >  (name overlap)
  >  (implementation
  >   (extension alt)
  >   (merlin_reader implementation))
  >  (interface
  >   (extension long.alt)
  >   (merlin_reader interface)))
  > EOF
  $ cat > overlap/dune <<EOF
  > (library (name sample))
  > EOF
  $ touch overlap/sample.alt overlap/sample.long.alt
  $ dune build --root overlap @check
  $ (cd overlap && merlin_configurations \
  >   _build/default/.merlin-conf/lib-sample \
  >   sample.pp.alt sample.pp.long.alt)
  sample.pp.alt: none
  sample.pp.long.alt: none
  $ for file in sample.alt sample.long.alt sample.pp.alt sample.pp.long.alt; do
  >   printf '%s: ' "$file"
  >   query_ocaml_merlin_pp "$PWD/overlap/$file" --root overlap \
  >     | grep -Eo '\(READER \([^)]*\)\)'
  > done
  sample.alt: (READER (implementation))
  sample.long.alt: (READER (implementation))
  sample.pp.alt: (READER (implementation))
  sample.pp.long.alt: (READER (implementation))

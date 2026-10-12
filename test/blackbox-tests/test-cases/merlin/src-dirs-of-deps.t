We create two libraries where one depends on the other. The dependency library
also has more than one src dir.

  $ make_dune_project 2.0
  $ mkdir -p lib1/sub
  $ cat >lib1/dune <<EOF
  > (include_subdirs unqualified)
  > (library (name lib1))
  > EOF
  $ touch lib1/sub/foo.ml
  $ touch lib1/bar.ml
  $ mkdir lib2
  $ cat >lib2/dune <<EOF
  > (library
  >  (name lib2)
  >  (libraries lib1)
  >  (modules ()))
  > EOF

  $ opam_prefix="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

  $ dune build lib2/.merlin-conf/lib-lib2
  $ dune ocaml merlin dump-config --format=json $PWD/lib2 | jq_dune -r '
  >   merlinEntry("Lib2")
  >   | merlinJsonEntryWithConfigNames(["STDLIB", "S", "UNIT_NAME"])'
  Lib2: _build/default/lib2/lib2
  ["STDLIB","/OPAM_PREFIX"]
  ["S","$TESTCASE_ROOT/lib1"]
  ["S","$TESTCASE_ROOT/lib1/sub"]
  ["S","$TESTCASE_ROOT/lib2"]
  ["UNIT_NAME","lib2"]
  Lib2: _build/default/lib2/lib2.ml-gen
  ["STDLIB","/OPAM_PREFIX"]
  ["S","$TESTCASE_ROOT/lib1"]
  ["S","$TESTCASE_ROOT/lib1/sub"]
  ["S","$TESTCASE_ROOT/lib2"]
  ["UNIT_NAME","lib2"]

Dependency source directories are reused by two consumers in two contexts.
Only the alternate context has a generated module in a third directory.

  $ mkdir lib3 lib1/alt_only
  $ cat >lib3/dune <<EOF
  > (library (name lib3) (libraries lib1) (modules ()))
  > EOF
  $ cat >lib1/alt_only/dune <<EOF
  > (rule
  >  (enabled_if (= %{context_name} alt))
  >  (target extra.ml)
  >  (action (write-file %{target} "")))
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.16)
  > (context (default))
  > (context (default (name alt) (generate_merlin_rules)))
  > EOF

  $ dune build _build/default/lib2/.merlin-conf/lib-lib2 \
  >   _build/default/lib3/.merlin-conf/lib-lib3 \
  >   _build/alt/lib2/.merlin-conf/lib-lib2 \
  >   _build/alt/lib3/.merlin-conf/lib-lib3

  $ show_dependency_dirs () {
  >   for ctx in default alt; do
  >     for lib in lib2 lib3; do
  >       printf '%s/%s: ' "$ctx" "$lib"
  >       dune ocaml merlin dump-config --context "$ctx" --format=json "$PWD/$lib" \
  >         | jq -c --arg root "$PWD/" '
  >           [.[0].config[] | select(.[0] == "S") | .[1] | ltrimstr($root)
  >            | select(. == "lib1" or startswith("lib1/"))]'
  >     done
  >   done
  > }
  $ show_dependency_dirs
  default/lib2: ["lib1","lib1/sub"]
  default/lib3: ["lib1","lib1/sub"]
  alt/lib2: ["lib1","lib1/alt_only","lib1/sub"]
  alt/lib3: ["lib1","lib1/alt_only","lib1/sub"]

Rebuilding after adding another source directory updates every configuration.

  $ mkdir lib1/new
  $ touch lib1/new/baz.ml
  $ dune build _build/default/lib2/.merlin-conf/lib-lib2 \
  >   _build/default/lib3/.merlin-conf/lib-lib3 \
  >   _build/alt/lib2/.merlin-conf/lib-lib2 \
  >   _build/alt/lib3/.merlin-conf/lib-lib3
  $ show_dependency_dirs
  default/lib2: ["lib1","lib1/new","lib1/sub"]
  default/lib3: ["lib1","lib1/new","lib1/sub"]
  alt/lib2: ["lib1","lib1/alt_only","lib1/new","lib1/sub"]
  alt/lib3: ["lib1","lib1/alt_only","lib1/new","lib1/sub"]

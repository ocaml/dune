Test that `dune tools install odoc` lets `dune build @doc` pick up the locked
odoc binary without requiring DUNE_CONFIG__LOCK_DEV_TOOL=enabled, mirroring
the behaviour of `dune tools install ocamlformat` + `dune fmt`.

This documents a current bug: `odoc_program` in src/dune_rules/odoc.ml only
inspects the compile-time `lock_dev_tools` flag and never falls back to an
`Fs_memo.dir_exists` check on the lockdir, unlike
`Ocamlformat.dev_tool_lock_dir_exists` in src/dune_rules/format_rules.ml.

  $ mkrepo
  $ make_mock_odoc_package
  $ mk_ocaml 5.2.0
  $ setup_odoc_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name foo)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let hello () = print_endline "hi"
  > EOF

  $ cat > foo.mli <<EOF
  > (** A greeting. *)
  > val hello : unit -> unit
  > EOF

  $ dune build

Helper to extract the odoc-related file dependencies of @doc:

  $ odoc_deps() {
  >   dune rules --deps --format=json @doc 2>/dev/null \
  >     | jq '[.. | objects | select(.File != null) | .File[1] | select(test("odoc"))] | unique'
  > }

Baseline before installing the dev-tool: no path under .dev-tool/odoc shows up
as a dep — dune isn't sourcing odoc from a lockdir (because there isn't one):

  $ odoc_deps
  [
    "_build/default/_doc/_html/sherlodoc.js",
    "_build/default/_doc/_odocls/foo/foo.odocl",
    "_build/default/_doc/_odocls/foo/page-index.odocl"
  ]

Install odoc via `dune tools install`, populating the dev-tool lockdir at
_build/.dev-tools.locks/odoc:

  $ dune tools install odoc
  Solution for _build/.dev-tools.locks/odoc:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - odoc.0.0.1

With DUNE_CONFIG__LOCK_DEV_TOOL=enabled, the locked odoc at
_build/_private/default/.dev-tool/odoc/target/bin/odoc now appears as a
dependency, so the build will invoke it instead of relying on PATH:

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled odoc_deps
  [
    "_build/_private/default/.dev-tool/odoc/target/bin/odoc",
    "_build/default/_doc/_html/sherlodoc.js",
    "_build/default/_doc/_odocls/foo/foo.odocl",
    "_build/default/_doc/_odocls/foo/page-index.odocl"
  ]

Without the flag, `dune build @doc` should also pick up the locked odoc (as
`dune fmt` does with ocamlformat). Currently the dev-tool path is missing from
the rule deps — odoc is sourced from PATH instead. This is the bug:

  $ odoc_deps
  [
    "_build/default/_doc/_html/sherlodoc.js",
    "_build/default/_doc/_odocls/foo/foo.odocl",
    "_build/default/_doc/_odocls/foo/page-index.odocl"
  ]

Dune delays major marking on small heaps and records the active tweak in the
first trace event.

  $ make_dune_project 3.22
  $ env -u OCAMLRUNPARAM -u CAMLRUNPARAM dune build
  $ dune trace cat | jq -s '
  >   first
  > | {name, cat, small_heap_limit: .args.gc_tweaks.small_heap_limit}'
  {
    "name": "init",
    "cat": "config",
    "small_heap_limit": 16777216
  }

Explicit runtime settings take precedence over Dune's defaults. The trace
records the effective tweak, including settings supplied by the user.

  $ env -u CAMLRUNPARAM OCAMLRUNPARAM=Xsmall_heap_limit=8388608 dune build
  $ dune trace cat | jq -s 'first.args.gc_tweaks.small_heap_limit'
  8388608

  $ env -u OCAMLRUNPARAM CAMLRUNPARAM=Xsmall_heap_limit=4194304 dune build
  $ dune trace cat | jq -s 'first.args.gc_tweaks.small_heap_limit'
  4194304

An empty runtime setting also disables Dune's defaults. No active tweaks are
reported in this case.

  $ env -u CAMLRUNPARAM OCAMLRUNPARAM= dune build
  $ dune trace cat | jq -s 'first.args | has("gc_tweaks")'
  false

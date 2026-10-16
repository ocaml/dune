Builds Merlin configuration targets explicitly.

  $ dune build .merlin-conf/lib-foo
  $ ls _build/default/.merlin-conf/lib-foo
  _build/default/.merlin-conf/lib-foo

Changing Merlin-only settings must not recompile an up-to-date library.

  $ rm dune-project
  $ make_dune_project 3.16
  $ echo 'let x = 1' > foo.ml
  $ dune build .merlin-conf/lib-foo foo.cma
  $ cat >> dune-project <<'EOF'
  > (dialect
  >  (name unused)
  >  (implementation
  >   (extension unused)
  >   (preprocess (run cat %{input-file}))
  >   (merlin_reader unused)))
  > EOF
  $ dune build .merlin-conf/lib-foo foo.cma
  $ dune trace cat | jq -s '
  >   [ .[]
  >     | select(.cat == "process" and .name == "start")
  >     | .args.process_args
  >     | select(index("-c")) ] | length'
  0
  $ dune ocaml merlin dump-config --format=json "$PWD" | jq -c '
  >   [.[] | .config[] | select(.[0] == "SUFFIX")] | unique'
  [["SUFFIX",".unused .unused"]]

Adding an unrelated library must update the existing configuration's indexes.

  $ dune ocaml merlin dump-config --format=json "$PWD" | jq -r '
  >   .[] | .config[] | select(.[0] == "INDEX") | .[1]' | sort -u
  $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index
  $ mkdir other
  $ echo '(library (name bar))' > other/dune
  $ touch other/bar.ml
  $ dune build .merlin-conf/lib-foo foo.cma
  $ dune ocaml merlin dump-config --format=json "$PWD" | jq -r '
  >   .[] | .config[] | select(.[0] == "INDEX") | .[1]' | sort -u
  $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index
  $TESTCASE_ROOT/_build/default/other/.bar.objs/cctx.ocaml-index

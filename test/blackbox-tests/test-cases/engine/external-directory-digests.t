A plain path in (deps) declares a file dependency, even when it names an external
directory. Dune hashes that directory recursively, but validates the cached
digest using only the directory's own stat. Editing an existing child leaves
that stat unchanged, so the rule incorrectly reuses stale output.

Contrast (source_tree inputs), which tracks child files and detects the edit.
The trace shows the missed invalidation; the output comparison shows stale data.

  $ export DUNE_TRACE=+cache
  $ mkdir -p external-inputs project/inputs
  $ echo original > external-inputs/input
  $ echo original > project/inputs/input
  $ external=$PWD/external-inputs
  $ cd project
  $ make_dune_project 3.26
  $ cat > dune <<EOF
  > (rule
  >  (target external.result)
  >  (deps "$external")
  >  (action
  >   (with-stdout-to external.result
  >    (run sh -c "cat '$external/input'"))))
  > (rule
  >  (target tree.result)
  >  (deps (source_tree inputs))
  >  (action
  >   (with-stdout-to tree.result
  >    (run sh -c "cat inputs/input"))))
  > EOF
  $ build() {
  >   dune build --root . --wait-for-filesystem-clock external.result tree.result
  > }
  $ misses() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | .args.target | split("/") | last
  >       | select(. == "external.result" or . == "tree.result")
  >     ] | sort
  >   '
  > }
  $ build
  $ misses
  ["external.result","tree.result"]

Only child contents change, leaving the external directory's statistics alone.
Both targets are requested, but only the source-tree rule is invalidated.

  $ echo changed > ../external-inputs/input
  $ echo changed > inputs/input
  $ build
  $ misses
  ["tree.result"]
  $ cat _build/default/external.result _build/default/tree.result
  original
  changed

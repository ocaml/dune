Reading a second empty include in a one-shot build does not change the rules.
Compare the number of computed Memo nodes rather than relying on a fixed total.

  $ make_dune_project 3.23
  $ touch a.inc b.inc
  $ nodes() {
  >   printf '(include a.inc)\n(include %s)\n(alias (name probe))\n' "$1" > dune
  >   dune build @probe &&
  >     dune trace cat | jq -er 'select(.name == "build-finish") | .args.memo.compute.nodes'
  > }
  $ same=$(nodes a.inc)
  $ distinct=$(nodes b.inc)

Neither include needs a watcher cell in a batch build.

  $ echo $((distinct - same))
  0

Targets can be promoted on to existing targets

  $ echo "(lang dune 3.20)" > dune-project

  $ mkdir subdir

  $ cat >dune <<EOF
  > (rule
  >  (action (with-stdout-to target (echo root)))
  >  (mode (promote (into subdir))))
  > EOF

  $ cat >subdir/dune <<EOF
  > (rule
  >  (action (with-stdout-to target (echo subdir))))
  > EOF

  $ dune build subdir/target target

  $ dune trace cat | jq 'select(.cat == "promote") | .args'
  {
    "src": "_build/default/target",
    "dst": "subdir/target"
  }

  $ function showFile() {
  > if [ -f "$1" ]; then
  >   echo "$1:"
  >   cat "$1"
  >   echo
  >   echo -n "---"
  > else
  >   echo "$1 does not exist"
  > fi
  > }

  $ function show() {
  > showFile subdir/target
  > echo
  > showFile _build/default/subdir/target
  > }

  $ show
  subdir/target:
  root
  ---
  _build/default/subdir/target:
  subdir
  ---

Worse still, promotions can be completely overlapping:

  $ rm -rf _build

  $ cat >subdir/dune <<EOF
  > (rule
  >  (mode promote)
  >  (action (with-stdout-to target (echo subdir))))
  > EOF

  $ dune build target subdir/target

  $ dune trace cat | jq 'select(.cat == "promote") | .args'
  {
    "src": "_build/default/subdir/target",
    "dst": "subdir/target"
  }

  $ show
  subdir/target:
  subdir
  ---
  _build/default/subdir/target:
  subdir
  ---

With the contents depending on rule execution order:

  $ rm -rf _build

  $ dune build target

  $ dune trace cat | jq 'select(.cat == "promote") | .args'
  {
    "src": "_build/default/target",
    "dst": "subdir/target"
  }

  $ show
  subdir/target:
  root
  ---
  _build/default/subdir/target does not exist

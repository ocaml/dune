Targets can be promoted on to existing targets

The test is not repproducible without this
  $ export DUNE_CONFIG__BACKGROUND_DIGESTS=disabledf

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
  Error: Invalid value for "DUNE_CONFIG__BACKGROUND_DIGESTS"
  only "enabled" and "disabled" are allowed
  [1]

  $ dune trace cat | jq 'select(.cat == "promote") | .args'

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
  subdir/target does not exist
  
  _build/default/subdir/target does not exist

Worse still, promotions can be completely overlapping:

  $ rm -rf _build

  $ cat >subdir/dune <<EOF
  > (rule
  >  (mode promote)
  >  (action (with-stdout-to target (echo subdir))))
  > EOF

  $ dune build target subdir/target
  Error: Invalid value for "DUNE_CONFIG__BACKGROUND_DIGESTS"
  only "enabled" and "disabled" are allowed
  [1]

  $ dune trace cat | jq 'select(.cat == "promote") | .args'

  $ show
  subdir/target does not exist
  
  _build/default/subdir/target does not exist

With the contents depending on rule execution order:

  $ rm -rf _build

  $ dune build target
  Error: Invalid value for "DUNE_CONFIG__BACKGROUND_DIGESTS"
  only "enabled" and "disabled" are allowed
  [1]

  $ dune trace cat | jq 'select(.cat == "promote") | .args'

  $ show
  subdir/target does not exist
  
  _build/default/subdir/target does not exist

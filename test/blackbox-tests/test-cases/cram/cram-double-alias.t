In this test we make sure a cram test is not run twice when it belongs to two separate
aliases that are being built together.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ cat >dune <<EOF
  > (cram
  >  (runtest_alias true)
  >  (alias this))
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo foo
  > EOF

  $ dune build @this @runtest
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]

Here we make sure that the cram test is only run once
  $ cat _build/log | grep dune_cram | sed 's/.*dune_cram_[0-9a-f]*_/dune_cram_HASH_/g'
  dune_cram_HASH_.cram.sh/main.sh)

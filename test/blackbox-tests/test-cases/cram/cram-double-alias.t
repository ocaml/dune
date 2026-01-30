In this test we make sure a cram test is not run twice when it belongs to two separate
aliases that are being built together.

  $ make_dune_project 3.17

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
  --- foo.t
  +++ foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]

Here we make sure that the cram test is only run once
  $ dune trace cat | jq -s 'include "dune"; [ .[] | processes | select(.args.categories == ["cram"]) ] | length'
  1

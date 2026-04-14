Demonstrate the dune clean command:

  $ make_dune_project "3.23"

First, show that we can clean per path

  $ cat >dune <<EOF
  > (rule
  >  (targets foo)
  >  (action (bash "touch foo && echo foo")))
  > (rule
  >  (targets bar)
  >  (action (bash "touch bar && echo bar")))
  > EOF

  $ function runtest() {
  > dune build ./foo ./bar | sort
  > }

  $ runtest
  foo
  bar

  $ runtest

  $ dune clean _build/default/foo

  $ runtest
  foo

We can clean the entire workspace to re-run everything:

  $ dune clean

  $ runtest
  foo
  bar

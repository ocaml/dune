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

Issue #2288: `dune clean` also accepts source directories, not just paths
inside the build directory, mirroring `dune build`. The corresponding artifacts
are removed from every build context.

  $ mkdir sub
  $ cat >sub/dune <<EOF
  > (rule
  >  (targets baz)
  >  (action (bash "touch baz && echo baz")))
  > EOF

  $ dune build sub
  baz
  $ test -e _build/default/sub/baz && echo built
  built

Cleaning the source directory removes its artifacts, leaving the rest intact:

  $ dune clean sub
  $ test -e _build/default/sub && echo present || echo removed
  removed
  $ test -e _build/default/foo && echo present || echo removed
  present

Cleaning the project root cleans the whole build context:

  $ dune clean .
  $ test -e _build/default && echo present || echo removed
  removed

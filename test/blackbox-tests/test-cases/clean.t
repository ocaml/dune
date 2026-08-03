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

Issue #2288: unlike `dune build`, `dune clean` does not accept source
directories (or package names); it only accepts paths inside the build
directory. So cleaning a source directory, or the project root, is rejected.

  $ mkdir sub
  $ cat >sub/dune <<EOF
  > (rule
  >  (targets baz)
  >  (action (bash "touch baz && echo baz")))
  > EOF

`dune build` accepts the source directory:

  $ dune build sub
  baz

But `dune clean` rejects it:

  $ dune clean sub
  Error: sub is not inside the build directory
  [1]

The project root is rejected too:

  $ dune clean .
  Error: . is not inside the build directory
  [1]

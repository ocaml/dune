A cram stanza uses different test names before and after dune language 3.25:

  $ make_dune_project 3.24

Before 3.25, [applies_to] matches the cram test name without the [.t] suffix:

  $ cat >dune <<EOF
  > (cram
  >  (applies_to foobar.t)
  >  (alias file-applies-match))
  > (cram
  >  (applies_to dirtest.t)
  >  (alias dir-applies-match))
  > EOF

  $ cat >foobar.t <<EOF
  >   $ echo foo
  > EOF

  $ mkdir dirtest.t
  $ cat >dirtest.t/run.t <<EOF
  >   $ echo dir
  > EOF

  $ dune show aliases > aliases 2>&1
  $ grep '^file-applies-match$' aliases
  [1]
  $ grep '^dir-applies-match$' aliases
  [1]

From 3.25 onward, the full cram test name is used:

  $ make_dune_project 3.25

  $ dune show aliases > aliases 2>&1
  $ grep '^file-applies-match$' aliases
  file-applies-match
  $ grep '^dir-applies-match$' aliases
  dir-applies-match

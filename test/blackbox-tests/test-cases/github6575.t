Given a working directory cram test:
  $ mkdir sometest.t
  $ cat >sometest.t/run.t <<EOF
  >   $ echo "foobar"
  >   foobar
  > EOF

  $ dune build @sometest

We turn it into a single-file test:
  $ mv sometest.t sometest.t.bak
  $ mv sometest.t.bak/run.t sometest.t
  $ rm -r sometest.t.bak

Dune detects the change:
  $ dune build @sometest

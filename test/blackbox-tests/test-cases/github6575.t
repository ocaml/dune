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

The other way. Turn the file test into a directory test:

  $ mv sometest.t sometest.t.bak
  $ mkdir sometest.t
  $ mv sometest.t.bak sometest.t/run.t

And this works:
  $ dune build @sometest

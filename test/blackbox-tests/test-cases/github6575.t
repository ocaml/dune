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

FIXME: Dune should detect the change:
  $ dune build @sometest
  Error: _build/default/sometest.t: Is a directory
  -> required by _build/default/sometest.t
  -> required by alias sometest
  [1]

After a clean it works as expected:
  $ dune clean
  $ dune build @sometest

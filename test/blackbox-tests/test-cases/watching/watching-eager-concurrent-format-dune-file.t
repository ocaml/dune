Regression test for running "dune format-dune-file" concurrently with a
watch server. The format command runs locally rather than through the RPC
server. It does not need to build anything, so it must not contend for the
workspace lock.

  $ echo '(lang dune 3.18)' > dune-project
  $ echo '(a(b c))' > input

  $ dune build --watch > .#dune-output 2>&1 &

Make sure the RPC server is properly started:
  $ dune rpc ping --wait
  Server appears to be responding normally

Formatting a dune file while watch mode owns the workspace lock succeeds:
  $ dune format-dune-file input
  (a
   (b c))

  $ dune shutdown
  $ wait


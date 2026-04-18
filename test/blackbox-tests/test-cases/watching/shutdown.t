Demonstrate what happens when the server is shut down in the middle of serving
a client.

  $ cat > dune-projec <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > mytest.t <<EOF
  >   $ sleep 1
  >   $ echo 'took too long'
  > EOF

  $ dune build -w &
  Success, waiting for filesystem changes...
  $ DUNE_PID=$!

  $ dune rpc ping --wait
  Server appears to be responding normally

  $ dune build --alias runtest &
  Error: Server returned error: 
  Connection terminated. This request will never receive a response. (error
  kind: Connection_dead)
  $ PID=$!

  $ dune rpc ping
  Server appears to be responding normally
  $ with_timeout dune shutdown
  $ wait_for_dune_exit

This allows us to observe the exit code which should be non-zero.
  $ wait $PID
  [1]

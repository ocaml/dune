Demonstrate what happens when the server is shut down in the middle of serving
a client.

  $ make_dune_project 3.21
  $ STARTED=$(mktemp)
  $ RELEASE=$(mktemp)
  $ OUTPUT=$(mktemp)
  $ rm "$STARTED" "$RELEASE"

  $ cat > mytest.t <<EOF
  >   $ touch "$STARTED"
  >   $ dune_cmd wait-for-file-to-appear "$RELEASE"
  >   $ echo 'took too long'
  > EOF

  $ dune build -w &
  Success, waiting for filesystem changes...
  $ DUNE_PID=$!

  $ dune rpc ping --wait
  Server appears to be responding normally

  $ dune build --alias runtest >"$OUTPUT" 2>&1 &
  $ PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"

  $ dune rpc ping
  Server appears to be responding normally
  $ with_timeout dune shutdown
  $ wait_for_dune_exit

This allows us to observe the exit code which should be non-zero.
  $ wait $PID
  [1]
  $ cat "$OUTPUT"
  Error: Server returned error: 
  Connection terminated. This request will never receive a response. (error
  kind: Connection_dead)

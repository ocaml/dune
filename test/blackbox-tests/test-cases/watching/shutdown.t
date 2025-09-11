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

  $ dune rpc ping --wait
  Server appears to be responding normally

  $ dune build --alias runtest &
  Error: { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Connection_dead
  }
  $ PID=$!

  $ dune rpc ping
  Server appears to be responding normally
  $ dune shutdown

This allows us to observe the exit code. Currently this is [0] which is wrong.
  $ wait $PID


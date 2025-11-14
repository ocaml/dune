Watch cancellation with concurrent RPC builds continues RPC builds to completion.

  $ . ./helpers.sh

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > EOF

  $ cat > input.txt << EOF
  > initial
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target watch-target.txt)
  >  (deps input.txt)
  >  (action (bash "sleep 0.2; cat input.txt > watch-target.txt")))
  > (rule
  >  (target rpc-target.txt)
  >  (action (bash "sleep 0.3; echo rpc > rpc-target.txt")))
  > (alias
  >  (name watch-build)
  >  (deps watch-target.txt))
  > (alias
  >  (name rpc-build)
  >  (deps rpc-target.txt))
  > EOF

  $ start_dune

Start slow RPC build, then trigger watch build and modify source to cancel it.

  $ build @rpc-build > rpc-output 2>&1 &
  $ RPC_PID=$!

  $ build @watch-build > watch-output 2>&1 &
  $ WATCH_PID=$!
  $ sleep 0.1
  $ cat > input.txt << EOF
  > modified
  > EOF

RPC build completes despite watch cancellation.

  $ wait $RPC_PID
  $ cat rpc-output
  Success

  $ wait $WATCH_PID || echo "Watch interrupted as expected"
  Watch interrupted as expected

  $ cat _build/default/rpc-target.txt
  rpc

Watch mode restarts and builds with modified input.

  $ build @watch-build
  Success

  $ cat _build/default/watch-target.txt
  modified

  $ stop_dune
  Success, waiting for filesystem changes...

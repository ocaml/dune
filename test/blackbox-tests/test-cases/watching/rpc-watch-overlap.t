RPC build during watch mode build executes concurrently without state corruption.

  $ . ./helpers.sh

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target slow.txt)
  >  (action (bash "sleep 0.3; echo slow > slow.txt")))
  > (rule
  >  (target fast.txt)
  >  (action (bash "echo fast > fast.txt")))
  > (alias
  >  (name slow-build)
  >  (deps slow.txt))
  > (alias
  >  (name fast-build)
  >  (deps fast.txt))
  > EOF

  $ start_dune

Trigger slow build via RPC, then start fast build while slow is running.

  $ build @slow-build > slow-output 2>&1 &
  $ SLOW_PID=$!

  $ sleep 0.1
  $ build @fast-build
  Success

Both builds should complete successfully.

  $ wait $SLOW_PID
  $ cat slow-output
  Success

  $ cat _build/default/slow.txt
  slow
  $ cat _build/default/fast.txt
  fast

  $ stop_dune
  Success, waiting for filesystem changes...

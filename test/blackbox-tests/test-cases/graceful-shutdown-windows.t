When dune is interrupted (e.g., Ctrl-C) on Windows while a build action is
running, dune should shut down cleanly. Previously,
kill_and_wait_for_all_processes called Process_watcher.wait_unix
unconditionally, which raises a Code_error on Windows because wait4 is not
available there.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries unix))
  > (rule
  >  (alias slow)
  >  (action (run ./main.exe)))
  > EOF

Create a program that ignores SIGINT (so it survives the interrupt sent to
dune), signals readiness, and sleeps indefinitely.

  $ cat > main.ml <<'EOF'
  > let () =
  >   let dir = Sys.getenv "TEST_DIR" in
  >   Sys.set_signal Sys.sigint Sys.Signal_ignore;
  >   let oc = open_out (Filename.concat dir "ready") in
  >   close_out oc;
  >   while true do Unix.sleepf 0.1 done
  > EOF

A wrapper script that runs dune build in the background, waits for the child to
be ready, sends SIGINT to dune, and checks that dune exits cleanly.

  $ export TEST_DIR=$PWD

  $ dune build @slow 2>/dev/null &

  $ DUNE_PID=$!

  $ wait_for_file ready

  $ kill -INT $DUNE_PID

  $ wait $DUNE_PID
  [130]

  $ dune trace cat | jq 'select(.name | startswith("process-")) | { name, args }'
  {
    "name": "process-cleanup-start",
    "args": {}
  }
  {
    "name": "process-cleanup-sigkill",
    "args": {}
  }
  {
    "name": "process-cleanup-finish",
    "args": {}
  }

When dune is interrupted (e.g., Ctrl-C), child processes running as build
actions should receive SIGTERM before SIGKILL so they can run cleanup handlers.

See https://github.com/ocaml/dune/issues/2445

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries unix))
  > (rule
  >  (alias slow)
  >  (action (run ./main.exe)))
  > EOF

Create a program that blocks SIGINT (so it isn't killed by the signal meant
for dune), installs a SIGTERM handler to write a marker file, signals
readiness, and sleeps.

  $ cat > main.ml <<'EOF'
  > let () =
  >   let dir = Sys.getenv "TEST_DIR" in
  >   (* Block SIGINT so we survive the interrupt sent to dune *)
  >   Sys.set_signal Sys.sigint Sys.Signal_ignore;
  >   Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
  >     let oc = open_out (Filename.concat dir "cleanup_ran") in
  >     output_string oc "cleanup ran\n";
  >     close_out oc;
  >     exit 0));
  >   let oc = open_out (Filename.concat dir "ready") in
  >   close_out oc;
  >   while true do Unix.sleepf 0.1 done
  > EOF

A wrapper script that runs dune build in the background, waits for the child to
be ready, sends SIGINT to dune, and checks if the cleanup handler ran.

  $ cat > run_test.sh <<'SCRIPT'
  > #!/bin/sh
  > export TEST_DIR=$PWD
  > rm -f ready cleanup_ran
  > dune build @slow 2>/dev/null &
  > DUNE_PID=$!
  > i=0; while [ $i -lt 200 ] && [ ! -e ready ]; do sleep 0.02; i=$((i+1)); done
  > if [ ! -e ready ]; then echo "child never became ready"; kill -9 $DUNE_PID 2>/dev/null; exit 1; fi
  > kill -INT $DUNE_PID
  > sleep 3
  > kill -9 $DUNE_PID 2>/dev/null
  > wait $DUNE_PID 2>/dev/null
  > if [ -f cleanup_ran ]; then
  >   cat cleanup_ran
  > else
  >   echo "cleanup did not run (child was killed without SIGTERM)"
  > fi
  > SCRIPT
  $ chmod +x run_test.sh

  $ sh run_test.sh
  cleanup ran

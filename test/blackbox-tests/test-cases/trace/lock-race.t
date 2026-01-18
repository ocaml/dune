Demonstrate that dune does not acquire the lock before writing to the trace file.
This is a bug where a second dune process that fails to acquire the lock will
still overwrite the trace file from the first process.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ ready=$(mktemp -d)/ready
  $ mkfifo $ready

Create a rule that will block the first dune process, holding the lock:

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (bash "echo ready > $ready; sleep 10")))
  > EOF

Start the first dune process in watch mode. It will acquire the lock and
hold it while running the rule:

  $ dune build @foo --watch >/dev/null &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Wait for the first process to start and acquire the lock:

  $ read line < $ready

Extract the PID from the first dune process's trace:

  $ pid1=$(dune trace cat | jq -r '.args.pid' | head -1)

Now try to run a second dune process with watch mode. The bug is that the second
dune will truncate the trace file BEFORE trying to acquire the lock. Even though
the second dune connects via RPC (and succeeds), it has already corrupted the trace:

  $ dune build @foo --watch 2>&1 | head -1
  Success

Check if the trace was overwritten. The PIDs should differ, demonstrating the bug:

  $ pid2=$(dune trace cat | jq -r '.args.pid' | head -1)
  $ if [ "$pid1" != "$pid2" ]; then
  >   echo "BUG: Trace was overwritten by second dune (PIDs differ)"
  > else
  >   echo "OK: Trace was not overwritten (PIDs match)"
  > fi
  BUG: Trace was overwritten by second dune (PIDs differ)

Clean up by killing the background dune process:

  $ dune shutdown
  $ wait

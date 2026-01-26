Demonstrate that dune acquires the lock before writing to the default trace file.
This test verifies that a second dune process will NOT overwrite the trace file
from the first process when it cannot acquire the lock.

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

  $ dune build @foo --watch &>/dev/null &

Wait for the first process to start and acquire the lock:

  $ read line < $ready

Extract the PID from the first dune process's trace:

  $ pid1=$(dune trace cat | jq -r '.args.pid' | head -1)

Now try to run a second dune process with watch mode. The second dune will NOT
truncate the trace file because it checks the lock first. It will connect via RPC
instead, leaving the original trace intact:

  $ dune build @foo --watch 2>&1 | head -1
  Success

Verify the trace was NOT overwritten. The PIDs should match:

  $ pid2=$(dune trace cat | jq -r '.args.pid' | head -1)
  $ if [ "$pid1" != "$pid2" ]; then
  >   echo "BUG: Trace was overwritten by second dune (PIDs differ)"
  > else
  >   echo "OK: Trace was not overwritten (PIDs match)"
  > fi
  OK: Trace was not overwritten (PIDs match)

Clean up by killing the background dune process:

  $ dune shutdown
  $ wait

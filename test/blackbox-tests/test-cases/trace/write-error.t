A failed trace write must not kill the signal watcher and leave the build
waiting forever for an exited child. The preload library fails writes containing
signal-received events, leaving initialization unaffected.

Use foreground timeouts so RPC clients receive SIGTERM only once, rather than
both directly and through their process group.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (universe))
  >  (action (run touch %{target})))
  > EOF
  $ export DUNE_TRACE=process
  $ timeout="$timeout --foreground"
  $ LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" \
  >   $timeout -s KILL 5 dune build result
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  [1]
  $ test -f _build/default/result
  [1]

A trace write can also fail during initialization. Using an action runner checks
that a failed trace is not subsequently shared with a worker. Capture the output
separately to check the build's exit status before filtering diagnostics.

  $ mkdir action-traces
  $ DUNE_ACTION_TRACE_DIR="$PWD/action-traces" \
  >   $timeout -s KILL 5 dune build --trace-file /dev/full \
  >   --action-runner result > initialization.log 2>&1
  $ grep -E '^(Error|Warning):' initialization.log
  Warning: Unable to write trace: No space left on device. Disabling tracing.

An incomplete trace must not be exported as an action trace.

  $ ls action-traces

A successful build does export a valid trace to the same directory.

  $ DUNE_ACTION_TRACE_DIR="$PWD/action-traces" dune build result
  $ test -f action-traces/*/trace.csexp
  $ dune trace cat --trace-file action-traces/*/trace.csexp |
  >   jq -r 'select(.name == "exit") | .name'
  exit

A first failure during scheduler cleanup must still be noticed after normal
work has completed. Fail only the signal watcher's final SIGUSR1 event.

  $ rm _build/default/result
  $ DUNE_TRACE_FAIL_CLEANUP=1 \
  >   LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" \
  >   $timeout -s KILL 5 dune build result > cleanup.log 2>&1
  [1]
  $ grep -E '^(Error|Warning):' cleanup.log
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  $ test -f _build/default/result
  $ test ! -s _build/.lock

A watch build stops after a trace failure and exits unsuccessfully.

  $ LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" start_dune
  $ build result > watch-request.log 2>&1
  [1]
  $ wait_for_dune_exit_with_timeout
  $ grep '^Warning:' .#dune-output
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  $ grep '^exit ' .#dune-output
  exit 1

A failure writing the final exit event is reported during shutdown and prevents
exporting the incomplete trace.

  $ mkdir exit-action-traces
  $ DUNE_TRACE_FAIL_AT_EXIT=1 \
  >   DUNE_ACTION_TRACE_DIR="$PWD/exit-action-traces" \
  >   LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" \
  >   $timeout -s KILL 5 dune build result > exit.log 2>&1
  $ grep -E '^(Warning:|\| Unix.Unix_error)' exit.log
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  $ ls exit-action-traces
  $ test ! -s _build/.lock

A trace failure before the console thread starts must not prevent the thread
from starting. Keep the progress backend enabled despite redirected output.

  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=enabled \
  >   DUNE_TRACE=thread DUNE_TRACE_FAIL_CONSOLE=1 \
  >   LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" \
  >   $timeout -s KILL 5 dune build --display progress result \
  >   > console.log 2>&1
  $ grep -o 'Warning: Unable to write trace:.*' console.log
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  $ test ! -s _build/.lock

A trace failure while another action is running must stop the build and clean up
its process group. Arm the failure only after the long-running child has started.

  $ marker="$TMPDIR/trace-failure-armed"
  $ child_pid="$TMPDIR/trace-failure-child.pid"
  $ cat > running.sh <<'EOF'
  > sleep 60 &
  > child=$!
  > echo "$child" > "$1"
  > touch "$2"
  > wait "$child"
  > touch "$3"
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (target running)
  >  (deps running.sh)
  >  (action (run bash %{dep:running.sh} "$child_pid" "$marker" %{target})))
  > (rule
  >  (target trigger)
  >  (action
  >   (bash "while ! test -f '$marker'; do sleep 0.01; done; touch trigger")))
  > EOF
  $ DUNE_TRACE_FAIL_AFTER="$marker" \
  >   LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" \
  >   $timeout -k 2 3 dune build -j 2 running trigger \
  >   > running.log 2>&1
  [1]
  $ grep -E '^(Error|Warning):' running.log
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  $ test -s "$child_pid"
  $ pid=$(cat "$child_pid")
  $ if kill -0 "$pid" 2>/dev/null; then
  >   kill -KILL "$pid"
  >   echo "child survived shutdown"
  > fi

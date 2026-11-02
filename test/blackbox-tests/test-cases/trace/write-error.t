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
  $ test -f _build/default/result

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

A watch build still handles build and shutdown requests after a trace failure.
The warning is reported only when the watch process exits.

  $ LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" start_dune
  $ build result
  Success
  $ build result
  Success
  $ grep '^Warning:' .#dune-output
  [1]
  $ stop_dune_quiet
  $ grep '^Warning:' .#dune-output
  Warning: Unable to write trace: No space left on device. Disabling tracing.
  $ grep '^exit ' .#dune-output
  [1]

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

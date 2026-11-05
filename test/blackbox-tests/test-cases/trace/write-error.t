A failed trace write must not kill the signal watcher and leave the build
waiting forever for an exited child. The preload library fails trace writes
from background threads, leaving initialization on the main thread unaffected.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (universe))
  >  (action (run touch %{target})))
  > EOF
  $ export DUNE_TRACE=process
  $ LD_PRELOAD="$PWD/../../utils/fail_trace_write.so" \
  >   $timeout --foreground -s KILL 5 dune build result
  Error: dune_trace_write(): No space left on device
  [137]
  $ test -f _build/default/result
  [1]

A trace write can also fail during initialization. Using an action runner checks
that a failed trace is not subsequently shared with a worker. Filter the shutdown
backtrace to avoid recording source locations.

  $ mkdir action-traces
  $ DUNE_ACTION_TRACE_DIR="$PWD/action-traces" \
  >   $timeout --foreground -s KILL 5 dune build --trace-file /dev/full \
  >   --action-runner result 2>&1 | grep -E '^(Error|Warning):'
  Error: dune_trace_write(): No space left on device
  [1]

An incomplete trace must not be exported as an action trace.

  $ ls action-traces

A successful build does export a valid trace to the same directory.

  $ DUNE_ACTION_TRACE_DIR="$PWD/action-traces" dune build result
  $ test -f action-traces/*/trace.csexp
  $ dune trace cat --trace-file action-traces/*/trace.csexp |
  >   jq -r 'select(.name == "exit") | .name'
  exit

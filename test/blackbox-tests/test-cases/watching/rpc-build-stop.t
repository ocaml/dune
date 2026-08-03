Minimal RPC watch shutdown after an RPC build.

  $ export DUNE_TRACE=rpc

  $ make_simple_rpc_watch_project

  $ start_dune
  $ build_quiet x

  $ stop_dune_quiet

  $ summarize_rpc_trace
  build start
  build stop
  shutdown start
  shutdown stop
  accept stop close

RPC build requests are cancelled if their client connection goes away.

  $ cancelled="$(mktemp -u)"
  $ cat > dune <<EOF
  > (rule
  >  (target slow)
  >  (action
  >   (bash "trap \"touch '$cancelled'; exit 0\" TERM; while true; do sleep 1; done")))
  > (rule
  >  (target fast)
  >  (action (with-stdout-to fast (echo fast))))
  > EOF

  $ start_dune
  $ "$timeout" 1 dune rpc build --wait slow > cancelled.out 2>&1
  [124]
  $ wait_for_file "$cancelled"
  $ build_quiet fast
  $ stop_dune_quiet

Concurrent RPC build requests both complete.

  $ release="$(mktemp -u)"
  $ cat > dune <<EOF
  > (rule
  >  (target a)
  >  (action
  >   (bash "while [ ! -e '$release' ]; do sleep 0.01; done; echo a > a")))
  > (rule
  >  (target b)
  >  (action
  >   (bash "while [ ! -e '$release' ]; do sleep 0.01; done; echo b > b")))
  > EOF

  $ start_dune
  $ dune rpc build --wait a > a.out 2>&1 &
  $ A=$!
  $ dune rpc build --wait b > b.out 2>&1 &
  $ B=$!
  $ touch "$release"
  $ wait_for_pid_to_exit_with_timeout "$A" 200 || (cat a.out; false)
  $ wait "$A"
  $ wait_for_pid_to_exit_with_timeout "$B" 200 || (cat b.out; false)
  $ wait "$B"
  $ cat a.out b.out | sort
  Success
  Success
  $ cat _build/default/a _build/default/b
  a
  b
  $ stop_dune_quiet

Shutdown cancels pending RPC build requests.

  $ cancelled="$(mktemp -u)"
  $ cat > dune <<EOF
  > (rule
  >  (target slow)
  >  (action
  >   (bash "trap \"touch '$cancelled'; exit 0\" TERM; while true; do sleep 1; done")))
  > EOF

  $ start_dune
  $ dune rpc build --wait slow > slow.out 2>&1 &
  $ SLOW=$!
  $ sleep 0.5
  $ shutdown_dune_quiet
  $ wait_for_dune_exit_with_timeout
  $ wait_for_file "$cancelled"
  $ wait_for_pid_to_exit_with_timeout "$SLOW" 200 || (cat slow.out; false)
  $ wait "$SLOW" || true

RPC build failures are reported to the client.

  $ cat > dune <<EOF
  > (rule
  >  (target fail)
  >  (action (bash "exit 1")))
  > EOF

  $ start_dune
  $ dune rpc build --wait fail
  Failure
  [1]
  $ stop_dune_quiet

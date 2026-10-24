Minimal RPC watch shutdown after an RPC build.

  $ # Needed when upgrading this test to Dune language 3.25:
  $ # export DUNE_CONFIG__LANDLOCK=disabled
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

RPC build requests are cancelled if their client connection goes away. Exercise
an unsandboxed action using a language version before user rules required
sandboxing.

  $ make_directory_targets_project 3.22
  $ cancelled="$(mktemp -u)"
  $ cat > dune <<EOF
  > (rule
  >  (targets slow partial-file (dir partial-dir))
  >  (deps (sandbox none))
  >  (action
  >   (progn
  >    (write-file slow partial)
  >    (write-file partial-file partial)
  >    (bash "mkdir -p partial-dir; echo partial > partial-dir/output;
  >           trap \"touch '$cancelled'; exit 0\" TERM;
  >           while true; do sleep 1; done"))))
  > (rule
  >  (target fast)
  >  (action (with-stdout-to fast (echo fast))))
  > EOF

  $ start_dune
  $ "$timeout" 1 dune rpc build --wait slow > cancelled.out 2>&1
  [124]
  $ with_timeout dune_cmd wait-for-file-to-appear "$cancelled"
  $ build_quiet fast

Cancellation currently leaves partial outputs in place, while a subsequent
successful rule keeps its completed output.

  $ dune_cmd exists _build/default/slow
  true
  $ dune_cmd exists _build/default/partial-file
  true
  $ dune_cmd exists _build/default/partial-dir
  true
  $ cat _build/default/fast
  fast
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

RPC build failures are reported to the client. Failed unsandboxed actions
currently retain their partial outputs too.

  $ cat > dune <<EOF
  > (rule
  >  (targets fail failed-file (dir failed-dir))
  >  (deps (sandbox none))
  >  (action
  >   (progn
  >    (write-file fail partial)
  >    (write-file failed-file partial)
  >    (bash "mkdir -p failed-dir; echo partial > failed-dir/output; exit 1"))))
  > EOF

  $ start_dune
  $ dune rpc build --wait fail
  Failure
  [1]
  $ dune_cmd exists _build/default/fail
  true
  $ dune_cmd exists _build/default/failed-file
  true
  $ dune_cmd exists _build/default/failed-dir
  true
  $ stop_dune_quiet

Forwarded builds display a rich status line once connected over RPC.

  $ setup_xdg_runtime_dir

  $ make_simple_rpc_watch_project

  $ export INSIDE_EMACS=1
  $ export DUNE_CONFIG__THREADED_CONSOLE=disabled

  $ start_dune --display progress
  $ : > .#dune-output
  $ with_timeout_quiet dune rpc ping
  $ tr '\r' '\n' < .#dune-output | grep -a -m 1 "\[rpc 0\]"
  [1]

  $ : > .#dune-output
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   with_timeout dune build --display progress x > output 2>&1
  $ tr '\r' '\n' < output | grep -m 1 "Connected to RPC server"
  Connected to RPC server
  $ tr '\r' '\n' < .#dune-output | grep -a -m 1 "\[rpc 1\]"
  [rpc 1]

  $ stop_dune_quiet

Batch builds display the build duration and parallelism alongside the client
count while their temporary RPC server is running.

  $ STARTED="$PWD/batch-started"
  $ RELEASE="$PWD/batch-release"
  $ CLIENT_CONNECTED="$PWD/batch-client-connected"
  $ cat > dune <<EOF
  > (rule
  >  (target batch-target)
  >  (action
  >   (progn
  >    (system "touch '$STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} ok))))
  > EOF

  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune build --display progress batch-target > batch-output 2>&1 &
  $ BATCH_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"

  $ dune_cmd hold-rpc-client _build/.rpc/dune "$CLIENT_CONNECTED" &
  $ CLIENT_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$CLIENT_CONNECTED"
  $ i=200
  $ while [ "$i" != 0 ]; do
  >   tr '\r' '\n' < batch-output | grep -a -q "\[rpc 1\]" && break
  >   i=$((i - 1))
  >   sleep 0.01
  > done
  $ tr '\r' '\n' < batch-output | grep -a -m 1 -o "\[rpc 1\]"
  [rpc 1]
  $ tr '\r' '\n' < batch-output \
  > | grep -a -E -m 1 -o "\[[0-9]+\.[0-9]s\] \[[0-9]+\.[0-9]x\]" \
  > | sed -E 's/\[[0-9]+\.[0-9]s\]/[BUILD DURATION]/; s/\[[0-9]+\.[0-9]x\]/[PARALLELISM]/'
  [BUILD DURATION] [PARALLELISM]

  $ touch "$RELEASE"
  $ if wait_for_pid_to_exit_with_timeout "$BATCH_PID" 200; then
  >   wait "$BATCH_PID"
  > else
  >   echo "batch build did not exit"
  > fi
  $ kill "$CLIENT_PID" 2>/dev/null || true
  $ wait "$CLIENT_PID" 2>/dev/null || true
  $ if kill -0 "$BATCH_PID" 2>/dev/null; then
  >   kill "$BATCH_PID"
  >   wait "$BATCH_PID" 2>/dev/null || true
  > fi

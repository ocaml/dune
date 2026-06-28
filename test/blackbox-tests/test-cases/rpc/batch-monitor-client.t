A batch build should not wait for an initialized RPC client with long-poll
requests in flight. dune monitor keeps progress, diagnostics, and running-job
polls open until the server disconnects it.

This is an end-to-end regression test for the batch RPC shutdown bug. It uses
the real CLI paths: the temporary RPC server started by a batch build, an
external dune monitor client, and the monitor's long-poll subscriptions. The
point is to catch the user-visible hang, not just the protocol-level behavior.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ STARTED="$PWD/monitor-started"
  $ RELEASE="$PWD/monitor-release"

  $ cat > dune <<EOF
  > (rule
  >  (target monitor-target)
  >  (action
  >   (progn
  >    (system "touch '$STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} ok))))
  > EOF

Force status-line output even though stderr is redirected. Dune disables the
progress display on non-tty output unless INSIDE_EMACS is set; the [rpc 1]
status-line marker below is our synchronization point that dune monitor has
connected before we release the build.

  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune build --display progress monitor-target > monitor-batch.out 2>&1 &
  $ BATCH_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"

  $ dune monitor --quit-on-disconnect > monitor.out 2>&1 &
  $ MONITOR_PID=$!
  $ i=200
  $ while [ "$i" != 0 ]; do
  >   tr '\r' '\n' < monitor-batch.out | grep -a -q "\[rpc 1\]" && break
  >   i=$((i - 1))
  >   sleep 0.01
  > done
  $ if [ "$i" = 0 ]; then
  >   echo "monitor did not connect"
  > fi

  $ touch "$RELEASE"
  $ if wait_for_pid_to_exit_with_timeout "$BATCH_PID" 200; then
  >   wait "$BATCH_PID"
  > else
  >   echo "batch build waited for monitor rpc client"
  > fi
  $ kill "$MONITOR_PID" 2>/dev/null || true
  $ wait "$MONITOR_PID" 2>/dev/null || true
  $ if kill -0 "$BATCH_PID" 2>/dev/null; then
  >   kill "$BATCH_PID"
  >   wait "$BATCH_PID" 2>/dev/null || true
  > fi

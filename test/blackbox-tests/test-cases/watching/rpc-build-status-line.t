Forwarded watch builds display a rich status line with the current run once
connected over RPC.

  $ # Needed when upgrading this test to Dune language 3.25:
  $ # export DUNE_CONFIG__LANDLOCK=disabled
  $ setup_xdg_runtime_dir

  $ make_simple_rpc_watch_project

  $ export INSIDE_EMACS=1
  $ export DUNE_CONFIG__THREADED_CONSOLE=disabled

  $ start_dune --display progress
  $ : > .#dune-output
  $ with_timeout_quiet dune rpc ping
  $ tr '\r' '\n' < .#dune-output | grep -a "\[rpc 0\]" | awk 'NR == 1'
  [1]

  $ : > .#dune-output
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   with_timeout dune build --display progress x > output 2>&1
  $ tr '\r' '\n' < output | grep "Connected to RPC server" | awk 'NR == 1'
  Connected to RPC server
  $ tr '\r' '\n' < .#dune-output | grep -a "\[rpc 1\]" | awk 'NR == 1'
  [rpc 1]
  $ tr '\r' '\n' < .#dune-output \
  > | grep -a -E -o "\[[0-9]+\.[0-9]s\] \[[0-9]+\.[0-9]x\] \[1\]" \
  > | sed -E 's/\[[0-9]+\.[0-9]s\]/[BUILD DURATION]/; s/\[[0-9]+\.[0-9]x\]/[PARALLELISM]/' \
  > | awk 'NR == 1'
  [BUILD DURATION] [PARALLELISM] [1]

The run number increments with the next watch build.

  $ : > .#dune-output
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   with_timeout dune build --display progress x > output 2>&1
  $ tr '\r' '\n' < output | grep "Connected to RPC server" | awk 'NR == 1'
  Connected to RPC server
  $ tr '\r' '\n' < .#dune-output \
  > | grep -a -E -o "\[[0-9]+\.[0-9]s\] \[[0-9]+\.[0-9]x\] \[2\]" \
  > | sed -E 's/\[[0-9]+\.[0-9]s\]/[BUILD DURATION]/; s/\[[0-9]+\.[0-9]x\]/[PARALLELISM]/' \
  > | awk 'NR == 1'
  [BUILD DURATION] [PARALLELISM] [2]

Forwarded builds display the progress of the build running on the server.

  $ STARTED="$PWD/progress-started"
  $ RELEASE="$PWD/progress-release"
  $ cat > dune <<EOF
  > (rule
  >  (target progress-target)
  >  (action
  >   (progn
  >    (system "touch '$STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} ok))))
  > EOF

  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune build --display progress progress-target > progress-output 2>&1 &
  $ PROGRESS_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"
  $ i=200
  $ while [ "$i" != 0 ]; do
  >   tr '\r' '\n' < progress-output | grep -a "Done:" >/dev/null && break
  >   i=$((i - 1))
  >   sleep 0.01
  > done
  $ tr '\r' '\n' < progress-output \
  > | grep -a "Connected to RPC server" \
  > | sed -E 's/ +$//; s/[0-9]+/N/g' \
  > | sort -u
  Connected to RPC server
  Connected to RPC server | Done: N% (N/N, N left)
  $ touch "$RELEASE"
  $ wait "$PROGRESS_PID"

When the server builds the requests of several clients at once, forwarded
builds report that the progress is shared with other builds.

  $ A_STARTED="$PWD/shared-a-started"
  $ RELEASE="$PWD/shared-release"
  $ cat > dune <<EOF
  > (rule
  >  (target shared-a)
  >  (action
  >   (progn
  >    (system "touch '$A_STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} ok))))
  > (rule
  >  (target shared-b)
  >  (action
  >   (progn
  >    (system "while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} ok))))
  > EOF

  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune build --display progress shared-a > shared-a-output 2>&1 &
  $ A_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$A_STARTED"
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune build --display progress shared-b > shared-b-output 2>&1 &
  $ B_PID=$!
  $ i=200
  $ while [ "$i" != 0 ]; do
  >   tr '\r' '\n' < shared-b-output | grep -a "shared with" >/dev/null && break
  >   i=$((i - 1))
  >   sleep 0.01
  > done
  $ tr '\r' '\n' < shared-b-output \
  > | grep -a -o "left) | shared with [0-9]* other builds*" \
  > | sort -u
  left) | shared with 1 other build
  $ touch "$RELEASE"
  $ wait "$A_PID"
  $ wait "$B_PID"

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
  >   tr '\r' '\n' < batch-output | grep -a "\[rpc 1\]" >/dev/null && break
  >   i=$((i - 1))
  >   sleep 0.01
  > done
  $ tr '\r' '\n' < batch-output | grep -a -o "\[rpc 1\]" | awk 'NR == 1'
  [rpc 1]
  $ tr '\r' '\n' < batch-output \
  > | grep -a -E -o "\[[0-9]+\.[0-9]s\] \[[0-9]+\.[0-9]x\]" \
  > | sed -E 's/\[[0-9]+\.[0-9]s\]/[BUILD DURATION]/; s/\[[0-9]+\.[0-9]x\]/[PARALLELISM]/' \
  > | awk 'NR == 1'
  [BUILD DURATION] [PARALLELISM]
  $ if tr '\r' '\n' < batch-output \
  > | grep -a -E "\[[0-9]+\.[0-9]s\] \[[0-9]+\.[0-9]x\] \[[0-9]+\]" >/dev/null; then
  >   echo "batch timing unexpectedly included a watch run"
  > else
  >   echo "batch timing has no watch run"
  > fi
  batch timing has no watch run

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

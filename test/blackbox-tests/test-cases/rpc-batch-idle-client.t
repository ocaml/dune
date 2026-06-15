A batch build should not wait for an idle RPC client to disconnect before
exiting. The helper below is that idle client: it connects, sends an incomplete
packet, writes a marker file, and then stays alive until this test kills it.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ STARTED="$PWD/started"
  $ RELEASE="$PWD/release"
  $ CLIENT_CONNECTED="$PWD/client-connected"

  $ cat > dune <<EOF
  > (rule
  >  (target ordinary-target)
  >  (action
  >   (progn
  >    (system "touch '$STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} ok))))
  > EOF

  $ dune build ordinary-target > batch.out 2>&1 &
  $ BATCH_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"

  $ dune_cmd hold-rpc-client _build/.rpc/dune "$CLIENT_CONNECTED" &
  $ CLIENT_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$CLIENT_CONNECTED"

The helper process remains connected while the build action is released below.
First, use a second RPC client as a barrier: if the batch RPC server is running,
this ping makes it accept the queued idle client; if no accept loop is running,
the ping times out and is ignored.

  $ $timeout 2 dune rpc ping --wait > /dev/null 2>&1 || true

  $ touch "$RELEASE"
  $ if wait_for_pid_to_exit_with_timeout "$BATCH_PID" 200; then
  >   wait "$BATCH_PID"
  > else
  >   echo "batch build waited for idle rpc client"
  > fi
  batch build waited for idle rpc client
  $ kill "$CLIENT_PID" 2>/dev/null || true
  $ wait "$CLIENT_PID" 2>/dev/null || true
  $ if kill -0 "$BATCH_PID" 2>/dev/null; then
  >   kill "$BATCH_PID"
  >   wait "$BATCH_PID" 2>/dev/null || true
  > fi
  $ cat batch.out

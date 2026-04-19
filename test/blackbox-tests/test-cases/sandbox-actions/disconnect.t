A worker disconnect in the middle of an exec request currently leaves the
build waiting forever.

  $ make_dune_project 3.23
  $ export TEST_DIR=$PWD
  $ cat > dune <<'EOF'
  > (rule
  >  (target break)
  >  (action
  >   (bash "touch \"$TEST_DIR/break.started\"; kill -9 $PPID; exit 0")))
  > EOF

The action starts and then kills the worker process that is serving the RPC
request. The main dune process keeps waiting for a response that will never
arrive.

  $ DUNE_JOBS=1 $timeout 2 dune build --sandbox-actions break > /dev/null 2>&1
  [124]

The marker proves that the action started running in the worker before the
request got stuck.

  $ file_status break.started
  break.started exists

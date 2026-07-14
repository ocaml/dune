A build request made while a batch build holds the global lock should report
that another Dune instance is running. Currently it is forwarded to the batch
RPC server, which does not support build requests, and returns an RPC code
error instead.

Use marker files to ensure that the first build is blocked in its action before
starting the second build.

  $ export DUNE_CONFIG__LANDLOCK=disabled
  $ make_dune_project 3.25
  $ STARTED="$PWD/started"
  $ RELEASE="$PWD/release"

  $ cat > dune <<EOF
  > (rule
  >  (target slow-target)
  >  (action
  >   (progn
  >    (bash "touch '$STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} slow))))
  > (rule
  >  (target fast-target)
  >  (action (write-file %{target} fast)))
  > EOF

  $ dune build slow-target > slow.out 2>&1 &
  $ SLOW_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"

  $ with_timeout dune build fast-target
  Error: Server returned error: 
  server error (error kind: Code_error)
  [1]

  $ touch "$RELEASE"
  $ wait "$SLOW_PID"
  $ cat slow.out

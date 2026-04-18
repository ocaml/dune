`--stop-on-first-error` is not yet reliable for actions already running in the
worker, so this currently times out instead of cancelling promptly.

  $ make_dune_project 3.23
  $ export TEST_DIR=$PWD
  $ cat > dune <<'EOF'
  > (rule
  >  (target slow)
  >  (action
  >   (bash "touch \"$TEST_DIR/slow-started\"; sleep 10; echo done > %{target}")))
  > 
  > (rule
  >  (target fail)
  >  (action
  >   (bash "while [ ! -f \"$TEST_DIR/slow-started\" ]; do sleep 0.1; done; touch \"$TEST_DIR/fail-started\"; echo failing-from-runner >&2; exit 1")))
  > EOF

  $ DUNE_JOBS=2 $timeout 2 dune build --sandbox-actions --stop-on-first-error slow fail \
  > > /dev/null 2>&1
  [124]

Both worker actions started, but the slow one never unwound cleanly.

  $ file_status slow-started
  slow-started exists

  $ file_status fail-started
  fail-started exists

  $ file_status _build/default/slow
  _build/default/slow missing

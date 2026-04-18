Known limitation: after an action-runner stop-on-first-error cancellation in
watch mode, the reused action runner does not recover cleanly for the next
build.

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
  >   (bash "while [ ! -f \"$TEST_DIR/slow-started\" ]; do sleep 0.1; done; echo failing-from-runner >&2; exit 1")))
  > EOF
  $ start_dune --action-runner --stop-on-first-error
  $ build_quiet slow fail >/dev/null 2>&1; code=$?; [ "$code" = 124 ]
  $ cat > dune <<'EOF'
  > (rule
  >  (target ok)
  >  (action (bash "echo ok > %{target}")))
  > EOF
  $ build_quiet ok >/dev/null 2>&1; code=$?; [ "$code" = 124 ]
  $ stop_dune_quiet

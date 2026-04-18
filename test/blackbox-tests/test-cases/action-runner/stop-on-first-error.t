`--stop-on-first-error` is not yet stable for actions already running in the
worker.

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
  $ DUNE_JOBS=2 $timeout 2 dune build --action-runner --stop-on-first-error \
  >   slow fail > /dev/null 2>&1; code=$?; [ "$code" = 1 ] || [ "$code" = 124 ]
  $ [ -e slow-started ]
  $ [ -e fail-started ]
  $ [ ! -e _build/default/slow ]

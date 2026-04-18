A single action runner is started lazily and reused even when dune is running
multiple processes.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action
  $ export TEST_DIR=$PWD
  $ cat > dune <<'EOF'
  > (rule
  >  (target single)
  >  (action (bash "echo single > %{target}")))
  > EOF

  $ DUNE_JOBS=2 dune build --action-runner single
  $ dune trace cat | jq -s 'include "dune"; runnerSpawnSummary'
  {
    "spawn": 1,
    "names": [
      "action-runner"
    ]
  }

  $ rm -f started-a started-b
  $ cat > dune <<'EOF'
  > (rule
  >  (target stamp)
  >  (action
  >   (progn
  >    (concurrent
  >     (bash "touch \"$TEST_DIR/started-a\"; while [ ! -f \"$TEST_DIR/started-b\" ]; do sleep 0.01; done")
  >     (bash "touch \"$TEST_DIR/started-b\"; while [ ! -f \"$TEST_DIR/started-a\" ]; do sleep 0.01; done"))
  >    (write-file %{target} done))))
  > EOF

  $ DUNE_JOBS=2 $timeout 5 dune build --action-runner stamp
  $ cat _build/default/stamp
  done
  $ dune trace cat | jq -s 'include "dune"; runnerSpawnSummary'
  {
    "spawn": 1,
    "names": [
      "action-runner"
    ]
  }

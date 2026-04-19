`--sandbox-actions` emits trace events for action-runner lifecycle.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target probe)
  >  (action (bash "echo from-runner > %{target}")))
  > EOF

  $ dune build probe
  $ dune trace cat | jq -s 'include "dune"; runnerEventSummary'
  {
    "spawn": 0,
    "connected": 0,
    "request_sent": false,
    "names": [],
    "spawn_pid_types": []
  }

  $ dune build --sandbox-actions probe
  $ dune trace cat | jq -s 'include "dune"; runnerEventSummary'
  {
    "spawn": 1,
    "connected": 1,
    "request_sent": true,
    "names": [
      "sandbox-actions"
    ],
    "spawn_pid_types": [
      "number"
    ]
  }

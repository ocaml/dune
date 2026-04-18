Failures from action-runner actions are reported back to the main process.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action
  $ cat > dune <<'EOF'
  > (rule
  >  (target fail)
  >  (action
  >   (bash "echo stderr-from-runner >&2; exit 1")))
  > EOF

  $ dune build --action-runner fail 2>&1 \
  > | sed -E 's/characters [0-9]+-[0-9]+/characters <REDACTED>/'
  File "dune", lines 1-4, characters <REDACTED>:
  1 | (rule
  2 |  (target fail)
  3 |  (action
  4 |   (bash "echo stderr-from-runner >&2; exit 1")))
  stderr-from-runner
  [1]
  $ dune trace cat | jq -s 'include "dune"; runnerRequestSummary'
  {
    "request_sent": true,
    "exec_start": 1,
    "names": [
      "action-runner"
    ]
  }

Stderr from successful actions remains separate from stdout.

  $ cat > dune <<'EOF'
  > (rule
  >  (target noisy)
  >  (action
  >   (progn
  >    (bash "echo stderr-from-runner >&2")
  >    (write-file %{target} done))))
  > EOF
  $ dune build --no-buffer --action-runner noisy >stdout 2>stderr
  $ cat stdout
  $ cat stderr
  stderr-from-runner

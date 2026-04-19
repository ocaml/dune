Failures from sandboxed actions are reported back to the main process.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target fail)
  >  (action
  >   (bash "echo stderr-from-runner >&2; exit 1")))
  > EOF

  $ dune build --sandbox-actions fail 2>&1 \
  > | sed -E 's/characters [0-9]+-[0-9]+/characters <REDACTED>/'
  File "dune", lines 1-4, characters <REDACTED>:
  1 | (rule
  2 |  (target fail)
  3 |  (action
  4 |   (bash "echo stderr-from-runner >&2; exit 1")))
  stderr-from-runner
  [1]

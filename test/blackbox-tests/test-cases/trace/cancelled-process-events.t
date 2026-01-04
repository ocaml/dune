Cancelled processes should emit both start and stop trace events.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune << EOF
  > (cram
  >  (timeout 0))
  > EOF

  $ cat > timeout.t << EOF
  >   $ sleep 1
  > EOF

  $ dune runtest timeout.t
  File "timeout.t", line 1, characters 0-0:
  Error: Cram test timed out while running command:
    $ sleep 1
  A time limit of 0.00s has been set in dune:2
  [1]

Verify the cancelled process has start, signal, and stop events in order:

  $ dune trace cat | jq -r '
  >   select((.cat == "process" and .name == "main.sh") or .name == "signal_sent")
  >   | if .name == "signal_sent" then "signal: \(.args.signal)" else .args.stage end'
  start
  signal: KILL
  stop

Process trace events are emitted at both start and stop. This test runs two
concurrent processes and verifies we see separate start/stop events.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

Two scripts synchronized via FIFOs - proc1 exits first, proc2 waits for it:

  $ cat > proc1.sh << 'EOF'
  > exec 3>fifo_alive
  > echo x > fifo_sync
  > EOF

  $ cat > proc2.sh << 'EOF'
  > exec 3<fifo_alive
  > read x < fifo_sync
  > read x <&3 || true
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (deps proc1.sh proc2.sh)
  >  (alias test-concurrent)
  >  (action
  >   (progn
  >    (run mkfifo fifo_alive fifo_sync)
  >    (concurrent
  >     (run sh proc1.sh)
  >     (run sh proc2.sh)))))
  > EOF

  $ dune build -j2 @test-concurrent --force

We now see separate start and stop events for each process:

  $ dune trace cat | jq -r 'select(.cat == "process" and .name == "sh") | .args.stage'
  start
  start
  stop
  stop

Chrome trace format has correct phases (b=begin, e=end):

  $ dune trace cat --chrome | jq -r '.[] | select(.cat == "process" and .name == "sh") | .ph' | sort | uniq
  b
  e

Each process ID has start (b) before stop (e):

  $ dune trace cat --chrome | jq '[.[] | select(.cat == "process" and .name == "sh")] | group_by(.id) | map([.[].ph]) | .[]'
  [
    "b",
    "e"
  ]
  [
    "b",
    "e"
  ]

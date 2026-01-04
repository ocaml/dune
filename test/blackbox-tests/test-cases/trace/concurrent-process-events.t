Process trace events are only emitted at completion, not at start. This test
runs two concurrent processes and verifies we only see "complete" events.

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

Both processes have duration (complete events), no separate start events:

  $ dune trace cat | jq 'select(.cat == "process" and .name == "sh") | .args.process_args[0], (.dur != null)'
  "proc1.sh"
  true
  "proc2.sh"
  true

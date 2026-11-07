Cram tests should have separate temporary directories. Order two tests explicitly
so that a producer writes a marker before a consumer checks its own TMPDIR.
Running with one job avoids any reliance on scheduling or timing.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (cram
  >  (applies_to producer.t)
  >  (alias produced))
  > (cram
  >  (applies_to consumer.t)
  >  (deps (alias produced)))
  > EOF
  $ cat > producer.t <<'EOF'
  >   $ echo 'written by producer.t' > "$TMPDIR/cram-isolation-marker"
  > EOF
  $ cat > consumer.t <<'EOF'
  >   $ if test -e "$TMPDIR/cram-isolation-marker"; then
  >   >   cat "$TMPDIR/cram-isolation-marker"
  >   > else
  >   >   echo isolated
  >   > fi
  >   isolated
  > EOF

The consumer unexpectedly sees the producer's marker, despite being a different
Cram test. This records the current isolation bug.

  $ dune runtest -j 1 --cache=disabled
  File "consumer.t", line 1, characters 0-0:
  --- consumer.t
  +++ consumer.t.corrected
  @@ -3,4 +3,4 @@
     > else
     >   echo isolated
     > fi
  -  isolated
  +  written by producer.t
  [1]

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

The runner's internal files are separate from the directory exposed to a test.
Reusing its filenames in TMPDIR must not corrupt scripts, output, or metadata.

  $ cat > runner-sh.t <<'EOF'
  >   $ echo 'captured before overwriting'
  >   captured before overwriting
  >   $ for file in main.sh 1.output cram.metadata; do
  >   >   echo 'not a Cram internal file' > "$TMPDIR/$file"
  >   > done
  >   $ echo 'still running'
  >   still running
  > EOF
  $ cp runner-sh.t runner-bash.t
  $ mkdir runner-dir.t
  $ cp runner-sh.t runner-dir.t/run.t
  $ cat >> dune <<'EOF'
  > (cram
  >  (applies_to runner-bash.t)
  >  (shell bash))
  > EOF
  $ dune runtest runner-sh.t runner-bash.t runner-dir.t -j 1

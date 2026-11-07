Cram tests should have separate temporary directories. Order two tests explicitly
so that a producer writes a marker before a consumer checks its own TMPDIR.
Running with one job avoids any reliance on scheduling or timing.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (cram
  >  (setup_scripts setup.sh))
  > (cram
  >  (applies_to producer.t)
  >  (alias produced))
  > (cram
  >  (applies_to consumer.t bash.t directory.t)
  >  (deps (alias produced)))
  > (cram
  >  (applies_to bash.t)
  >  (shell bash))
  > EOF
  $ cat > setup.sh <<'EOF'
  > test -d "$TMPDIR" || exit 1
  > echo ready > "$TMPDIR/setup-marker"
  > EOF
  $ cat > producer.t <<'EOF'
  >   $ echo 'written by producer.t' > "$TMPDIR/cram-isolation-marker"
  > EOF
  $ cat > consumer.t <<'EOF'
  >   $ cat "$TMPDIR/setup-marker"
  >   ready
  >   $ echo "$TMPDIR"
  >   $TMPDIR
  >   $ if test -e "$TMPDIR/cram-isolation-marker"; then
  >   >   cat "$TMPDIR/cram-isolation-marker"
  >   > else
  >   >   echo isolated
  >   > fi
  >   isolated
  > EOF

Cover both shells and directory tests. Each consumer has an existing, writable
TMPDIR before setup scripts run, and none can see the producer's marker.

  $ cp consumer.t bash.t
  $ mkdir directory.t
  $ cp consumer.t directory.t/run.t
  $ dune runtest -j 1 --cache=disabled

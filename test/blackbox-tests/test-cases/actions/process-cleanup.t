A failed spawn releases captured output while Dune remains running.

  $ make_dune_project 3.0
  $ mkdir "$TMPDIR/process-cleanup"
  $ export TMPDIR="$TMPDIR/process-cleanup"

  $ cat >disappearing-executable <<'EOF'
  > #!/bin/sh
  > echo unexpected
  > EOF
  $ chmod 755 disappearing-executable

  $ cat >dune <<'EOF'
  > (rule
  >  (alias spawn-failure)
  >  (deps disappearing-executable)
  >  (action
  >   (progn
  >    (run rm -f disappearing-executable)
  >    (run ./disappearing-executable))))
  > EOF

File-watcher invalidations can restart the build and create extra capture files,
so disable the watcher to observe exactly one failed spawn.

  $ start_dune --file-watcher manual
  $ build "(alias spawn-failure)"
  Failure
  [1]

  $ for f in "$TMPDIR"/dune*stdout "$TMPDIR"/dune*stderr; do
  >   if test -e "$f"; then
  >     basename "$f" | sed -E 's/^dune.*(stdout|stderr)$/dune<ID>\1/'
  >   fi
  > done

  $ stop_dune_quiet

Replay preserves raw process output and shell-compatible failure statuses.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (target raw)
  >  (action
  >   (run sh -c "echo raw-stdout; echo raw-stderr >&2; exit 9")))
  > 
  > (rule
  >  (target signaled)
  >  (action (run sh -c "kill -TERM $$")))
  > 
  > (rule
  >  (target accepted-signaled)
  >  (action
  >   (with-accepted-exit-codes 143
  >    (run sh -c "kill -TERM $$"))))
  > EOF

Replay returns raw stdout, stderr, and non-zero status.

  $ dune shell --sandbox=copy _build/default/raw -- sh -c '
  > "$DUNE_SHELL/dune-run" >replay.stdout 2>replay.stderr
  > status=$?
  > echo "status: $status"
  > printf "stdout: "; cat replay.stdout
  > printf "stderr: "; cat replay.stderr
  > ' 2>raw-entry.stderr
  status: 9
  stdout: raw-stdout
  stderr: raw-stderr

Signal termination is returned as the shell-compatible status without a Dune
process diagnostic. A numeric accepted-exit predicate does not accidentally
accept a signal.

  $ for target in signaled accepted-signaled; do
  >   dune shell --sandbox=copy _build/default/$target -- sh -c '
  >     "$DUNE_SHELL/dune-run" >signal.stdout 2>signal.stderr
  >     echo "status: $?"
  >     test ! -s signal.stderr && echo "stderr: empty"
  >   ' 2>"$target-entry.stderr"
  > done
  status: 143
  stderr: empty
  status: 143
  stderr: empty

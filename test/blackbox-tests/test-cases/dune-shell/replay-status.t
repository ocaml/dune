Replay preserves raw process output and shell-compatible failure statuses.

  $ make_dune_project 3.23

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
  > 
  > (rule
  >  (target exit-seven)
  >  (action (write-file %{target} seven)))
  > EOF

Replay returns raw stdout, stderr, and non-zero status.

  $ dune shell --sandbox=copy _build/default/raw -- sh -c '
  > "$DUNE_SHELL/dune-run" >replay.stdout 2>replay.stderr
  > status=$?
  > echo "status: $status"
  > printf "stdout: "; cat replay.stdout
  > printf "stderr: "; cat replay.stderr
  > '
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
  >   '
  > done
  status: 143
  stderr: empty
  status: 143
  stderr: empty

Command mode propagates the command's own exit status directly, without a
wrapped Dune process error, and still cleans up the sandbox and metadata.

  $ export ROOT=$PWD
  $ dune shell --sandbox=copy _build/default/exit-seven -- sh -c '
  > printf "%s\n" "$PWD" > "$ROOT/nonzero-sandbox"
  > printf "%s\n" "$DUNE_SHELL" > "$ROOT/nonzero-metadata"
  > exit 7
  > ' \
  >   >exit.stdout 2>exit.stderr
  [7]
  $ grep '^Error:' exit.stderr || echo "no wrapped error"
  no wrapped error
  $ test ! -e "$(cat nonzero-sandbox)" && echo "nonzero-sandbox: cleaned"
  nonzero-sandbox: cleaned
  $ test ! -e "$(cat nonzero-metadata)" && echo "nonzero-metadata: cleaned"
  nonzero-metadata: cleaned

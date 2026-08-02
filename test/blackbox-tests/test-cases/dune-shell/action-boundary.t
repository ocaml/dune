`dune shell` drives the ordinary live build to the selected rule's action
boundary. The selected action is replaced by the shell while the initiating
build, global lock, action locks, and canonical sandbox stay alive.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

  $ mkdir sub
  $ cat > sub/dune <<'EOF'
  > (env
  >  (_
  >   (env-vars (DUNE_SHELL_TEST_ENV prepared))))
  > 
  > (rule
  >  (target prepared-input)
  >  (deps source.txt)
  >  (action (copy source.txt prepared-input)))
  > 
  > (rule
  >  (targets out action-cwd)
  >  (deps prepared-input)
  >  (action
  >   (progn
  >    (with-stdout-to out (cat prepared-input))
  >    (with-stdout-to action-cwd (run pwd))
  >    (run sh -c ": > \"$DUNE_SHELL_ENTRY_BEACON\""))))
  > EOF

  $ echo initial > sub/source.txt

Even when the selected target and all of its prerequisites are current, entry
must still reach its action boundary, remove its old targets, and stop before
executing the selected action.

  $ export DUNE_SHELL_ENTRY_BEACON=$PWD/selected-action-ran
  $ dune build --sandbox=copy _build/default/sub/out
  $ rm selected-action-ran
  $ dune shell --sandbox=copy _build/default/sub/out -- sh -c '
  > if test ! -e out && test ! -e action-cwd; then
  >   echo "current-target: suspended"
  > else
  >   echo "current-target: already ran"
  > fi
  > if test ! -e "$DUNE_SHELL_ENTRY_BEACON"; then
  >   echo "current-side-effect: suspended"
  > else
  >   echo "current-side-effect: already ran"
  > fi
  > printf "current-dependency: "; cat prepared-input
  > ' 2>current-entry.stderr
  current-target: suspended
  current-side-effect: suspended
  current-dependency: initial

Now change an input of the generated prerequisite. Entry must rebuild that
prerequisite and again stop at the selected action boundary.

  $ echo refreshed > sub/source.txt

The shell starts in the digest-addressed canonical action sandbox. The lock
file is still owned by the shell's parent Dune process, demonstrating that the
initiating build has not exited. Replay remains in the prepared sandbox, runs
repeatedly, and never rebuilds after post-entry workspace changes.

  $ export ROOT=$PWD
  $ DUNE_BINARY=$(command -v dune)
  $ export DUNE_BINARY
  $ dune shell --sandbox=copy _build/default/sub/out -- sh -c '
  > digest=$(cat "$DUNE_SHELL/rule-digest")
  > if test "$PWD" = "$ROOT/_build/.sandbox/$digest/default/sub"; then
  >   echo "canonical-cwd: exact"
  > else
  >   echo "canonical-cwd: unexpected: $PWD"
  > fi
  > if test "$(cat "$ROOT/_build/.lock")" = "$PPID"; then
  >   echo "live-build-lock: held"
  > else
  >   echo "live-build-lock: unexpected"
  > fi
  > if "$DUNE_BINARY" build --root "$ROOT" _build/default/sub/prepared-input \
  >      >"$ROOT/second-dune.stdout" 2>"$ROOT/second-dune.stderr"; then
  >   echo "advisory-lock: unexpectedly available"
  > else
  >   echo "advisory-lock: held"
  > fi
  > if test -e out || test -e action-cwd; then
  >   echo "selected-action: already ran"
  > else
  >   echo "selected-action: suspended"
  > fi
  > if test -e "$DUNE_SHELL_ENTRY_BEACON"; then
  >   echo "selected-side-effect: already ran"
  > else
  >   echo "selected-side-effect: suspended"
  > fi
  > printf "prepared-dependency: "; cat prepared-input
  > if test -x "$DUNE_SHELL/dune-run" &&
  >    test -f "$DUNE_SHELL/action.sexp"; then
  >   echo "session-tools: ready"
  > else
  >   echo "session-tools: missing"
  > fi
  > printf "prepared-env: %s\n" "$DUNE_SHELL_TEST_ENV"
  > "$DUNE_SHELL/dune-run"
  > printf "first-replay: "; cat out
  > test "$(cat action-cwd)" = "$PWD" && echo "replay-cwd: exact"
  > echo post-entry > "$ROOT/sub/source.txt"
  > "$DUNE_SHELL/dune-run"
  > printf "snapshot-replay: "; cat out
  > sed -i "s/(cat prepared-input)/(echo edited-action)/" \
  >   "$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > printf "edited-replay: "; cat out; echo
  > printf "%s\n" "(with-stdout-to edited-cwd (bash \"pwd\"))" \
  >   >"$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > test "$(cat edited-cwd)" = "$PWD" && echo "edited-wrapper-cwd: exact"
  > printf "%s\n" "$PWD" > "$ROOT/session-sandbox"
  > printf "%s\n" "$DUNE_SHELL" > "$ROOT/session-metadata"
  > ' 2>shell-entry.stderr
  canonical-cwd: exact
  live-build-lock: held
  advisory-lock: held
  selected-action: suspended
  selected-side-effect: suspended
  prepared-dependency: refreshed
  session-tools: ready
  prepared-env: prepared
  first-replay: refreshed
  replay-cwd: exact
  snapshot-replay: refreshed
  edited-replay: edited-action
  edited-wrapper-cwd: exact

Dune prints nothing at startup. In command mode the output belongs entirely to
the user's command; the per-topic session detail lives behind the sourced
helper commands and the metadata files.

  $ test ! -s shell-entry.stderr && echo "command-startup: silent"
  command-startup: silent

No replay result was extracted to the real build directory. Returning from
the command lets the same live `Sandbox.with_` clean the canonical sandbox and
the command's disposable metadata.

  $ test ! -e _build/default/sub/out && echo "build-target: absent"
  build-target: absent
  $ test ! -e "$(cat session-sandbox)" && echo "sandbox-cleanup: complete"
  sandbox-cleanup: complete
  $ test ! -e "$(cat session-metadata)" && echo "metadata-cleanup: complete"
  metadata-cleanup: complete
  $ dune build _build/default/sub/prepared-input
  $ echo "live-build-lock-after-exit: released"
  live-build-lock-after-exit: released

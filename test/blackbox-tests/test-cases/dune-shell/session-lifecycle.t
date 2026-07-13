During a session the initiating build stays alive: its global lock, the
rule's action locks, and the canonical digest sandbox are owned by the live
build until the shell exits. Replay runs in that sandbox against the prepared
snapshot, and everything is cleaned up afterwards.

  $ make_dune_project 3.23

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
  $ export DUNE_SHELL_ENTRY_BEACON=$PWD/selected-action-ran
  $ dune build --sandbox=copy _build/default/sub/out
  $ rm selected-action-ran
  $ export ROOT=$PWD
  $ DUNE_BINARY=$(command -v dune)
  $ export DUNE_BINARY
  $ dune shell --sandbox=copy _build/default/sub/out -- sh -c '
  > printf "canonical-cwd: "; echo "$PWD"
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
  > test -x "$DUNE_SHELL/dune-run" &&
  >   test -f "$DUNE_SHELL/action.sexp" && echo "session-tools: ready"
  > test ! -e "$DUNE_SHELL_ENTRY_BEACON" && echo "selected-action: suspended"
  > printf "prepared-dependency: "; cat prepared-input
  > printf "prepared-env: %s\n" "$DUNE_SHELL_TEST_ENV"
  > "$DUNE_SHELL/dune-run"
  > printf "first-replay: "; cat out
  > test "$(cat action-cwd)" = "$PWD" && echo "replay-cwd: exact"
  > echo post-entry > "$ROOT/sub/source.txt"
  > "$DUNE_SHELL/dune-run"
  > printf "snapshot-replay: "; cat out
  > sed -i.bak "s/(cat prepared-input)/(echo edited-action)/" \
  >   "$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > printf "edited-replay: "; cat out; echo
  > printf "%s\n" "(with-stdout-to edited-cwd (bash \"pwd\"))" \
  >   >"$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > test "$(cat edited-cwd)" = "$PWD" && echo "edited-wrapper-cwd: exact"
  > printf "%s\n" "$PWD" > "$ROOT/session-sandbox"
  > printf "%s\n" "$DUNE_SHELL" > "$ROOT/session-metadata"
  > ' 2>shell-entry.stderr | censor
  canonical-cwd: $PWD/_build/.sandbox/$DIGEST/default/sub
  live-build-lock: held
  advisory-lock: held
  session-tools: ready
  selected-action: suspended
  prepared-dependency: initial
  prepared-env: prepared
  first-replay: initial
  replay-cwd: exact
  snapshot-replay: initial
  edited-replay: edited-action
  edited-wrapper-cwd: exact

Dune prints nothing at startup. In command mode the output belongs entirely to
the user's command; the per-topic session detail lives behind the sourced
helper commands and the metadata files.

  $ test ! -s shell-entry.stderr && echo "command-startup: silent"
  command-startup: silent

Returning from the command ends the live [Sandbox.with_] scope: the canonical
sandbox and the disposable session metadata are removed, replay output is not
extracted into the real build directory, and the build-directory lock is
released.

  $ test ! -e _build/default/sub/out && echo "build-target: absent"
  build-target: absent
  $ test ! -e "$(cat session-sandbox)" && echo "sandbox-cleanup: complete"
  sandbox-cleanup: complete
  $ test ! -e "$(cat session-metadata)" && echo "metadata-cleanup: complete"
  metadata-cleanup: complete
  $ dune build _build/default/sub/prepared-input
  $ echo "live-build-lock-after-exit: released"
  live-build-lock-after-exit: released

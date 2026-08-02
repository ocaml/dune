The shell and replay share Dune's prepared action environment while keeping
shell-control variables out of the replayed action.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (env-vars (DUNE_SHELL_TEST_ENV prepared))))
  > 
  > (rule
  >  (targets (dir output-dir))
  >  (action
  >   (progn
  >    (run mkdir -p output-dir)
  >    (chdir output-dir (run sh -c "echo directory-target > value")))))
  > 
  > (rule
  >  (target action-env)
  >  (action
  >   (with-stdout-to %{target}
  >    (run sh -c
  >     "if test -n \"${DUNE_SHELL+x}\"; then
  >        echo present
  >      else
  >        echo absent
  >      fi"))))
  > 
  > (rule
  >  (target prepared-env)
  >  (action
  >   (with-stdout-to %{target}
  >    (run sh -c "printf '%s\\n' \"$DUNE_SHELL_TEST_ENV\""))))
  > 
  > (rule
  >  (target prepared-temp)
  >  (action
  >   (setenv TMPDIR action-overridden-temp
  >    (with-stdout-to %{target} (run sh -c "printf '%s\\n' \"$TMPDIR\"")))))
  > EOF

Directory targets and action `chdir` paths are recreated after replay clears
the target.

  $ dune shell --sandbox=copy _build/default/output-dir -- sh -c '
  > "$DUNE_SHELL/dune-run"
  > printf "directory-target: "; cat output-dir/value
  > ' 2>directory-target-entry.stderr
  directory-target: directory-target

Shell-control variables are present for debugging but absent from the action;
ordinary action-environment layers are present in both.

  $ dune shell --sandbox=copy _build/default/action-env -- sh -c '
  > test -n "$DUNE_SHELL" && echo "debug-command: DUNE_SHELL is set"
  > "$DUNE_SHELL/dune-run"
  > printf "action: DUNE_SHELL is "; cat action-env
  > ' 2>action-env-entry.stderr
  debug-command: DUNE_SHELL is set
  action: DUNE_SHELL is absent

  $ dune shell --sandbox=copy _build/default/prepared-env -- sh -c '
  > printf "debug-command: %s\n" "$DUNE_SHELL_TEST_ENV"
  > "$DUNE_SHELL/dune-run"
  > printf "replayed-action: "; cat prepared-env
  > ' 2>prepared-env-entry.stderr
  debug-command: prepared
  replayed-action: prepared

Dune's execution-time temporary directory injection takes precedence over an
action-level [setenv TMPDIR], just as it does for an ordinary action process.
The shell and replay use that same initiating-build directory.

  $ dune shell --sandbox=copy _build/default/prepared-temp -- sh -c '
  > prepared_temp=$TMPDIR
  > if test -n "$TMPDIR" && test "$TMPDIR" != action-overridden-temp; then
  >   echo "shell-temp: exact"
  > else
  >   echo "shell-temp: unexpected: $TMPDIR"
  > fi
  > "$DUNE_SHELL/dune-run"
  > if test "$(cat prepared-temp)" = "$prepared_temp"; then
  >   echo "replay-temp: exact"
  > else
  >   echo "replay-temp: unexpected: $(cat prepared-temp)"
  > fi
  > ' 2>prepared-temp-entry.stderr
  shell-temp: exact
  replay-temp: exact

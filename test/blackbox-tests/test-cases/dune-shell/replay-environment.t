The shell and replay share Dune's prepared action environment while keeping
shell-control variables out of the replayed action.

  $ make_dune_project 3.23

  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (env-vars (DUNE_SHELL_TEST_ENV prepared))))
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

Shell-control variables are present for debugging but absent from the action;
ordinary action-environment layers are present in both.

  $ dune shell --sandbox=copy _build/default/action-env -- sh -c '
  > test -n "$DUNE_SHELL" && echo "debug-command: DUNE_SHELL is set"
  > "$DUNE_SHELL/dune-run"
  > printf "action: DUNE_SHELL is "; cat action-env
  > '
  debug-command: DUNE_SHELL is set
  action: DUNE_SHELL is absent

  $ dune shell --sandbox=copy _build/default/prepared-env -- sh -c '
  > printf "debug-command: %s\n" "$DUNE_SHELL_TEST_ENV"
  > "$DUNE_SHELL/dune-run"
  > printf "replayed-action: "; cat prepared-env
  > '
  debug-command: prepared
  replayed-action: prepared

Dune's execution-time temporary directory injection takes precedence over an
action-level [setenv TMPDIR], just as it does for an ordinary action process.
The shell and replay use that same initiating-build directory.

  $ dune shell --sandbox=copy _build/default/prepared-temp -- sh -c '
  > prepared_temp=$TMPDIR
  > test -n "$TMPDIR" &&
  >   test "$TMPDIR" != action-overridden-temp && echo "shell-temp: exact"
  > "$DUNE_SHELL/dune-run"
  > test "$(cat prepared-temp)" = "$prepared_temp" && echo "replay-temp: exact"
  > '
  shell-temp: exact
  replay-temp: exact

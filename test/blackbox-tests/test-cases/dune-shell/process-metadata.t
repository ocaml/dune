Direct process metadata preserves prepared executables, arguments,
environments, and unresolved programs for inspection and debugging.

  $ make_dune_project 3.23

  $ cat > dune <<'EOF'
  > (rule
  >  (target local-tool)
  >  (deps local-tool-source)
  >  (action (copy local-tool-source local-tool)))
  > 
  > (rule
  >  (target local-tool-output)
  >  (deps local-tool)
  >  (action
  >   (setenv LEADING_ACTION_ENV visible
  >    (with-stdout-to %{target} (run ./local-tool)))))
  > 
  > (rule
  >  (target bash-output)
  >  (action (bash "echo bash-replay > bash-output")))
  > EOF

  $ cat > local-tool-source <<'EOF'
  > #!/bin/sh
  > printf 'same-directory-tool:%s\n' "${LEADING_ACTION_ENV-unset}"
  > EOF
  $ chmod +x local-tool-source

A resolved executable in the action directory retains its [./] path instead
of being searched through PATH. Leading action [setenv] modifiers are also
part of the shell's prepared environment and of the recorded command
environment.

  $ dune shell --sandbox=copy _build/default/local-tool-output -- sh -c '
  > printf "leading-action-env: %s\n" "$LEADING_ACTION_ENV"
  > grep -q "(run ./local-tool)" "$DUNE_SHELL/action.sexp" &&
  >   echo "local-program: exact path"
  > test "$(cat "$DUNE_SHELL/command.cwd")" = "$PWD" &&
  >   test -f "$DUNE_SHELL/command.argv.csexp" &&
  >   test -f "$DUNE_SHELL/command.env.csexp" &&
  >   grep -q "local-tool" "$DUNE_SHELL/command" &&
  >   grep -q "^LEADING_ACTION_ENV=visible$" "$DUNE_SHELL/command.env" &&
  >   echo "direct-process-metadata: exact"
  > "$DUNE_SHELL/dune-run"
  > printf "local-program-output: "; cat local-tool-output
  > '
  leading-action-env: visible
  local-program: exact path
  direct-process-metadata: exact
  local-program-output: same-directory-tool:visible

A [(bash ...)] action is also a literal single-process invocation. Its exact
shell executable, arguments, cwd, and environment are exposed alongside a
[(run ...)] action's metadata.

  $ dune shell --sandbox=copy _build/default/bash-output -- sh -c '
  > test -f "$DUNE_SHELL/command" &&
  >   test -f "$DUNE_SHELL/command.argv.csexp" &&
  >   test -f "$DUNE_SHELL/command.cwd" &&
  >   test -f "$DUNE_SHELL/command.env.csexp" &&
  >   grep -q "bash" "$DUNE_SHELL/command" &&
  >   grep -q "pipefail" "$DUNE_SHELL/command.argv.csexp" &&
  >   echo "bash-process-metadata: exact"
  > "$DUNE_SHELL/dune-run"
  > printf "bash-output: "; cat bash-output
  > '
  bash-process-metadata: exact
  bash-output: bash-replay

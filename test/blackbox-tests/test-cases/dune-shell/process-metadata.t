Direct process metadata preserves prepared executables, arguments,
environments, and unresolved programs for inspection and editing.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

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
  >  (target repaired-tool-output)
  >  (deps local-tool)
  >  (action
  >   (with-stdout-to %{target} (run not-in-prepared-path))))
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
part of the shell's prepared environment.

  $ dune shell --sandbox=copy _build/default/local-tool-output -- sh -c '
  > printf "leading-action-env: %s\n" "$LEADING_ACTION_ENV"
  > if grep -q "(run ./local-tool)" "$DUNE_SHELL/action.sexp"; then
  >   echo "local-program: exact path"
  > else
  >   echo "local-program: changed"
  > fi
  > if test "$(cat "$DUNE_SHELL/command.cwd")" = "$PWD" &&
  >    test -f "$DUNE_SHELL/command.argv.csexp" &&
  >    test -f "$DUNE_SHELL/command.env.csexp" &&
  >    grep -q "local-tool" "$DUNE_SHELL/command" &&
  >    grep -q "^LEADING_ACTION_ENV=visible$" "$DUNE_SHELL/command.env"; then
  >   echo "direct-process-metadata: exact"
  > else
  >   echo "direct-process-metadata: missing"
  > fi
  > "$DUNE_SHELL/dune-run"
  > printf "local-program-output: "; cat local-tool-output
  > ' 2>local-program-entry.stderr
  leading-action-env: visible
  local-program: exact path
  direct-process-metadata: exact
  local-program-output: same-directory-tool:visible

A [(bash ...)] action is also a literal single-process invocation. Its exact
shell executable, arguments, cwd, and environment are exposed alongside a
[(run ...)] action's metadata.

  $ dune shell --sandbox=copy _build/default/bash-output -- sh -c '
  > if test -f "$DUNE_SHELL/command" &&
  >    test -f "$DUNE_SHELL/command.argv.csexp" &&
  >    test -f "$DUNE_SHELL/command.cwd" &&
  >    test -f "$DUNE_SHELL/command.env.csexp" &&
  >    grep -q "bash" "$DUNE_SHELL/command" &&
  >    grep -q "pipefail" "$DUNE_SHELL/command.argv.csexp"; then
  >   echo "bash-process-metadata: exact"
  > else
  >   echo "bash-process-metadata: missing"
  > fi
  > "$DUNE_SHELL/dune-run"
  > printf "bash-output: "; cat bash-output
  > ' 2>bash-entry.stderr
  bash-process-metadata: exact
  bash-output: bash-replay

Removing the leading [setenv] wrapper from the editable action also removes
that variable from replay; the shell's entry environment is not reused as the
action's base environment.

  $ dune shell --sandbox=copy _build/default/local-tool-output -- sh -c '
  > printf "%s\n" "(with-stdout-to local-tool-output (run ./local-tool))" \
  >   >"$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > printf "edited-leading-env: "; cat local-tool-output
  > ' 2>edited-leading-env-entry.stderr
  edited-leading-env: same-directory-tool:unset

An unresolved prepared program remains unresolved in metadata rather than
being searched again. Editing that explicit form into the already prepared
local executable then makes replay succeed.

  $ dune shell --sandbox=copy _build/default/repaired-tool-output -- sh -c '
  > if grep -q "unresolved-program.*not-in-prepared-path" \
  >      "$DUNE_SHELL/action.sexp"; then
  >   echo "unresolved-program: preserved"
  > else
  >   echo "unresolved-program: lost"
  > fi
  > sed -i "s/(unresolved-program [^ ]* not-in-prepared-path)/.\/local-tool/" \
  >   "$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > printf "repaired-program-output: "; cat repaired-tool-output
  > ' 2>repaired-program-entry.stderr
  unresolved-program: preserved
  repaired-program-output: same-directory-tool:unset

Editing a prepared action cannot bypass the first-version concurrent-action
restriction.

  $ dune shell --sandbox=copy _build/default/local-tool-output -- sh -c '
  > printf "%s\n" "(concurrent (run sh -c \"exit 2\") (run sh -c \"exit 3\"))" \
  >   >"$DUNE_SHELL/action.sexp"
  > if "$DUNE_SHELL/dune-run" >concurrent.stdout 2>concurrent.stderr; then
  >   echo "edited-concurrent: accepted"
  > else
  >   echo "edited-concurrent: rejected"
  > fi
  > grep -qi "concurrent" concurrent.stderr &&
  >   echo "edited-concurrent-error: actionable"
  > ' 2>edited-concurrent-entry.stderr
  edited-concurrent: rejected
  edited-concurrent-error: actionable

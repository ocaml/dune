The expanded action is exposed as an editable file: editing
[$DUNE_SHELL/action.sexp] changes what [run] replays, without rebuilding
dependencies or re-resolving programs.

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
  >  (target repaired-tool-output)
  >  (deps local-tool)
  >  (action
  >   (with-stdout-to %{target} (run not-in-prepared-path))))
  > EOF

  $ cat > local-tool-source <<'EOF'
  > #!/bin/sh
  > printf 'same-directory-tool:%s\n' "${LEADING_ACTION_ENV-unset}"
  > EOF
  $ chmod +x local-tool-source

Removing the leading [setenv] wrapper from the editable action also removes
that variable from replay; the shell's entry environment is not reused as the
action's base environment.

  $ dune shell --sandbox=copy _build/default/local-tool-output -- sh -c '
  > printf "%s\n" "(with-stdout-to local-tool-output (run ./local-tool))" \
  >   >"$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > printf "edited-leading-env: "; cat local-tool-output
  > '
  edited-leading-env: same-directory-tool:unset

An unresolved prepared program remains unresolved in metadata rather than
being searched again. Editing that explicit form into the already prepared
local executable then makes replay succeed.

  $ dune shell --sandbox=copy _build/default/repaired-tool-output -- sh -c '
  > grep -q "unresolved-program.*not-in-prepared-path" \
  >   "$DUNE_SHELL/action.sexp" && echo "unresolved-program: preserved"
  > sed -i.bak "s/(unresolved-program [^ ]* not-in-prepared-path)/.\/local-tool/" \
  >   "$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run"
  > printf "repaired-program-output: "; cat repaired-tool-output
  > '
  unresolved-program: preserved
  repaired-program-output: same-directory-tool:unset

An edited concurrent group replays with ordinary build semantics: both
branches run, and a failing branch's raw status is returned without a Dune
process diagnostic.

  $ dune shell --sandbox=copy _build/default/local-tool-output -- sh -c '
  > printf "%s\n" "(concurrent (run /bin/sh -c \"exit 2\") (echo done))" \
  >   >"$DUNE_SHELL/action.sexp"
  > "$DUNE_SHELL/dune-run" >concurrent.stdout 2>concurrent.stderr
  > echo "edited-concurrent-status: $?"
  > test ! -s concurrent.stderr && echo "edited-concurrent-error: silent"
  > '
  edited-concurrent-status: 2
  edited-concurrent-error: silent

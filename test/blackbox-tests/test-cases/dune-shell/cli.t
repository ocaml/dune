`dune shell TARGET -- COMMAND...` is the non-interactive form.  Aliases,
dynamic actions, and action extensions do not yet have a faithful
single-rule session, so the first version rejects them explicitly instead of
silently approximating them.

  $ make_dune_project_with_extension 3.23 action-plugin 0.1

  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (env-vars (DUNE_SANDBOX none))))
  > 
  > (rule
  >  (target ordinary)
  >  (action (write-file %{target} ordinary)))
  > 
  > (rule
  >  (targets configured configured-cwd)
  >  (action
  >   (progn
  >    (with-stdout-to configured (echo %{profile}))
  >    (with-stdout-to configured-cwd (run pwd)))))
  > 
  > (rule
  >  (alias grouped)
  >  (action (echo alias-action)))
  > 
  > (rule
  >  (target dynamic)
  >  (action
  >   (progn
  >    (dynamic-run dune_cmd exit-code 0)
  >    (write-file %{target} dynamic))))
  > 
  > (rule
  >  (target universe)
  >  (deps (universe))
  >  (action (write-file %{target} universe)))
  > 
  > (rule
  >  (targets concurrent-a concurrent-b)
  >  (action
  >   (concurrent
  >    (write-file concurrent-a a)
  >    (write-file concurrent-b b))))
  > 
  > (rule
  >  (target extension)
  >  (action
  >   (progn
  >    (echo before-extension)
  >    (format-dune-file dune extension))))
  > EOF

The command after [--] runs in the prepared environment.

  $ dune shell _build/default/ordinary -- sh -c 'echo command-ran' 2>/dev/null
  command-ran

The live build preserves the configuration that selected the rule, including
a non-default profile and build directory.  The shell and replayed action use
the same exact mapped working directory.  Disabling dune's global lock for the
runner does not matter because [dune-run] executes only the prepared action; it
does not start another build system.

  $ ROOT=$PWD dune shell --profile release --build-dir _custom --sandbox=copy \
  >   _custom/default/configured -- sh -c '
  > case "$PWD" in
  >   "$ROOT"/_custom/.sandbox/*/default)
  >     echo "configured-directory: exact" ;;
  >   *) echo "configured-directory: unexpected: $PWD" ;;
  > esac
  > rm -f configured
  > DUNE_CONFIG__GLOBAL_LOCK=disabled "$DUNE_SHELL/dune-run"
  > printf "configured-profile: "; cat configured; echo
  > if [ "$(cat configured-cwd)" = "$PWD" ]; then
  >   echo "replay-directory: exact"
  > else
  >   echo "replay-directory: unexpected: $(cat configured-cwd)"
  > fi
  > '
  configured-directory: exact
  configured-profile: release
  replay-directory: exact

The initiating build system and its canonical sandbox operation stay live for
the session; the ordinary build-directory lock is acquired like any other
build command.
Action runners change process placement and isolation.  Until a session can
reproduce that configuration without breaking debugger ancestry, it is
rejected explicitly.

  $ if dune shell --sandbox=copy --sandbox-actions \
  >      _build/default/ordinary -- true \
  >      >sandbox-actions.stdout 2>sandbox-actions.stderr; then
  >   echo "sandbox-actions: unexpectedly accepted"
  > else
  >   echo "sandbox-actions: rejected"
  > fi
  sandbox-actions: rejected
  $ grep -Eqi 'external action runners' sandbox-actions.stderr &&
  >   echo "sandbox-actions-error: specific"
  sandbox-actions-error: specific

An alias can contain several actions and therefore needs a selection
mechanism before it can be supported.

  $ if dune shell @grouped -- true >alias.stdout 2>alias.stderr; then
  >   echo "alias: unexpectedly accepted"
  > else
  >   echo "alias: rejected"
  > fi
  alias: rejected
  $ grep -qi 'Aliases select multiple actions' alias.stderr &&
  >   echo "alias-error: specific"
  alias-error: specific

A dynamic action discovers dependencies while it runs, which is incompatible
with preparing a complete session in advance.

  $ if dune shell _build/default/dynamic -- true \
  >      >dynamic.stdout 2>dynamic.stderr; then
  >   echo "dynamic: unexpectedly accepted"
  > else
  >   echo "dynamic: rejected"
  > fi
  dynamic: rejected
  $ grep -qi 'dynamic actions' dynamic.stderr &&
  >   echo "dynamic-error: specific"
  dynamic-error: specific

A universe dependency contributes no concrete paths to the prepared
environment, exactly as it does to a real build's sandbox, so the session can
be prepared and the action replayed.

  $ dune shell _build/default/universe -- sh -c '
  > "$DUNE_SHELL/dune-run"
  > printf "universe-target: "; cat universe
  > '
  universe-target: universe
Concurrent groups replay with ordinary build semantics; when branches fail,
the first failing branch's raw status is returned.

  $ dune shell _build/default/concurrent-a -- sh -c '
  > "$DUNE_SHELL/dune-run"
  > printf "concurrent-a: "; cat concurrent-a; echo
  > printf "concurrent-b: "; cat concurrent-b; echo
  > '
  concurrent-a: a
  concurrent-b: b
Static action extensions do not yet expose the raw-status and side-effect
contract required by replay.  Nested extensions are rejected rather than
silently using ordinary build semantics.

  $ if dune shell _build/default/extension -- true \
  >      >extension.stdout 2>extension.stderr; then
  >   echo "extension: unexpectedly accepted"
  > else
  >   echo "extension: rejected"
  > fi
  extension: rejected
  $ grep -qi 'action extension' extension.stderr &&
  >   echo "extension-error: specific"
  extension-error: specific

Watch mode restarts the build between runs, which is incompatible with a
long-lived prepared session, so it is rejected explicitly.

  $ if dune shell -w _build/default/ordinary -- true \
  >      >watch.stdout 2>watch.stderr; then
  >   echo "watch: unexpectedly accepted"
  > else
  >   echo "watch: rejected"
  > fi
  watch: rejected
  $ grep -qi 'watch mode' watch.stderr &&
  >   echo "watch-error: specific"
  watch-error: specific

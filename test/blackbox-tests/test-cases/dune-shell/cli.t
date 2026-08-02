`dune shell TARGET -- COMMAND...` is the non-interactive form.  Aliases and
dynamic actions do not yet have a faithful single-rule session, so the first
version rejects them explicitly instead of silently approximating them.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using action-plugin 0.1)
  > EOF

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
  > ' 2>configured.stderr
  configured-directory: exact
  configured-profile: release
  replay-directory: exact

The initiating build system and its canonical sandbox operation stay live for
the session, so dune's ordinary global build-directory lock is required.

  $ if env DUNE_CONFIG__GLOBAL_LOCK=disabled \
  >      dune shell _build/default/ordinary -- true \
  >      >global-lock.stdout 2>global-lock.stderr; then
  >   echo "global-lock: unexpectedly accepted"
  > else
  >   echo "global-lock: rejected"
  > fi
  global-lock: rejected
  $ if grep -Eqi 'global.*lock|lock.*disabled|requires.*lock' \
  >      global-lock.stderr; then
  >   echo "global-lock-error: specific"
  > else
  >   echo "global-lock-error: not specific"
  > fi
  global-lock-error: specific

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
  $ if grep -Eqi 'action runner|action-runner|sandbox-actions' \
  >      sandbox-actions.stderr; then
  >   echo "sandbox-actions-error: specific"
  > else
  >   echo "sandbox-actions-error: not specific"
  > fi
  sandbox-actions-error: specific

An alias can contain several actions and therefore needs a selection
mechanism before it can be supported.

  $ if dune shell @grouped -- true >alias.stdout 2>alias.stderr; then
  >   echo "alias: unexpectedly accepted"
  > else
  >   echo "alias: rejected"
  > fi
  alias: rejected
  $ if grep -qi alias alias.stderr &&
  >    grep -Eqi 'concrete|not supported|unsupported|cannot|selection' \
  >      alias.stderr; then
  >   echo "alias-error: specific"
  > else
  >   echo "alias-error: not specific"
  > fi
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
  $ if grep -qi dynamic dynamic.stderr &&
  >    grep -Eqi 'not support|unsupported|cannot' dynamic.stderr; then
  >   echo "dynamic-error: specific"
  > else
  >   echo "dynamic-error: not specific"
  > fi
  dynamic-error: specific

A universe dependency cannot describe a fixed prepared file layout.

  $ if dune shell _build/default/universe -- true \
  >      >universe.stdout 2>universe.stderr; then
  >   echo "universe: unexpectedly accepted"
  > else
  >   echo "universe: rejected"
  > fi
  universe: rejected
  $ if grep -qi universe universe.stderr &&
  >    grep -Eqi 'fixed|not support|cannot|arbitrary' universe.stderr; then
  >   echo "universe-error: specific"
  > else
  >   echo "universe-error: not specific"
  > fi
  universe-error: specific

Concurrent actions need a defined raw-status aggregation policy before replay
can expose P5 faithfully.

  $ if dune shell _build/default/concurrent-a -- true \
  >      >concurrent.stdout 2>concurrent.stderr; then
  >   echo "concurrent: unexpectedly accepted"
  > else
  >   echo "concurrent: rejected"
  > fi
  concurrent: rejected
  $ if grep -qi concurrent concurrent.stderr &&
  >    grep -Eqi 'exit status|not support|policy' concurrent.stderr; then
  >   echo "concurrent-error: specific"
  > else
  >   echo "concurrent-error: not specific"
  > fi
  concurrent-error: specific

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
  $ if grep -qi extension extension.stderr &&
  >    grep -Eqi 'not support|replay|contract' extension.stderr; then
  >   echo "extension-error: specific"
  > else
  >   echo "extension-error: not specific"
  > fi
  extension-error: specific

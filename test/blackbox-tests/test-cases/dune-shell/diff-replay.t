Replay preserves diff behavior without applying Dune's post-action promotion
effects.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (target corrected)
  >  (action
  >   (progn
  >    (write-file corrected actual)
  >    (diff expected corrected))))
  > EOF
  $ echo expected > expected

Replay of a diff neither changes its source input nor registers a pending
promotion.

  $ dune shell --sandbox=copy _build/default/corrected -- sh -c '
  > if "$DUNE_SHELL/dune-run" >diff.stdout 2>diff.stderr; then
  >   echo "diff-replay: unexpectedly succeeded"
  > else
  >   echo "diff-replay: failed"
  > fi
  > ' 2>diff-entry.stderr
  diff-replay: failed
  $ printf "diff-source: "; cat expected
  diff-source: expected
  $ if test -n "$(dune promotion list)"; then
  >   echo "diff-promotion: registered"
  > else
  >   echo "diff-promotion: absent"
  > fi
  diff-promotion: absent

Replay retains the initiating invocation's configured diff command.

  $ export DIFF_MARKER=$PWD/custom-diff-ran
  $ cat > custom-diff <<'EOF'
  > #!/bin/sh
  > : > "$DIFF_MARKER"
  > exit 1
  > EOF
  $ chmod +x custom-diff
  $ dune shell --diff-command "$PWD/custom-diff" --sandbox=copy \
  >   _build/default/corrected -- sh -c '
  > "$DUNE_SHELL/dune-run" >/dev/null 2>&1 || :
  > ' 2>custom-diff-entry.stderr
  $ test -e custom-diff-ran && echo "diff-command: preserved"
  diff-command: preserved

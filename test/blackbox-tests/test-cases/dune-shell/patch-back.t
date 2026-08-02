Patch-back-source-tree rules intentionally propagate action writes to the
source tree.  Exact replay would therefore perform implicit source updates,
whose policy is deferred, so dune shell rejects them with a specific
explanation.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (target out)
  >  (deps (sandbox patch_back_source_tree))
  >  (action (with-stdout-to %{target} (echo out))))
  > EOF

  $ if dune shell _build/default/out -- true \
  >      >shell.stdout 2>shell.stderr; then
  >   echo "patch-back: unexpectedly accepted"
  > else
  >   echo "patch-back: rejected"
  > fi
  patch-back: rejected
  $ if grep -Eqi 'patch[-_ ]?back' shell.stderr &&
  >    grep -qi 'source' shell.stderr; then
  >   echo "patch-back-error: specific"
  > else
  >   echo "patch-back-error: not specific"
  > fi
  patch-back-error: specific

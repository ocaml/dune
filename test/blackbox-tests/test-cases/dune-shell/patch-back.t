Patch-back-source-tree rules intentionally propagate action writes to the
source tree.  Exact replay would therefore perform implicit source updates,
whose policy is deferred, so dune shell rejects them with a specific
explanation.

  $ make_dune_project 3.23

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
  $ grep -qi 'patch-back-source-tree' shell.stderr &&
  >   echo "patch-back-error: specific"
  patch-back-error: specific

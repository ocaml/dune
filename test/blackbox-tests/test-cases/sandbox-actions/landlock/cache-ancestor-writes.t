When Landlock protects the shared cache, the worker cannot create new entries
directly in ancestors of the protected cache directory.

  $ make_dune_project 3.23
  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db"
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (action
  >   (bash
  >    "if touch \"$DUNE_CACHE_ROOT/ancestor-marker\" 2>/dev/null; then echo wrote > %{target}; else echo blocked > %{target}; fi")))
  > EOF

Normal actions can create entries directly in the cache root.

  $ dune build result
  $ cat _build/default/result
  wrote
  $ test -e "$DUNE_CACHE_ROOT/ancestor-marker" && echo present || echo missing
  present

The Landlock backend protects `$DUNE_CACHE_ROOT/db`, and the deny algorithm
does not grant directory write rights on `$DUNE_CACHE_ROOT` itself.

  $ rm -f "$DUNE_CACHE_ROOT/ancestor-marker"
  $ rm -rf _build
  $ dune build --sandbox-actions --sandbox-actions-backend=landlock result
  $ cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/ancestor-marker" && echo present || echo missing
  missing

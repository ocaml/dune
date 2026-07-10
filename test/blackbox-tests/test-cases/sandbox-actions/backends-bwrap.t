The --sandbox-actions-backend flag can select bwrap explicitly or stack bwrap
outside Landlock.

  $ make_dune_project 3.23
  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ echo "$DUNE_CACHE_ROOT" > cache-root-name
  $ mkdir -p "$DUNE_CACHE_ROOT/db"
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps cache-root-name)
  >  (action
  >   (bash
  >    "if touch \"$DUNE_CACHE_ROOT/db/runner-marker\" 2>/dev/null; then echo wrote > %{target}; else echo blocked > %{target}; fi")))
  > EOF

By default, Landlock is selected when available, even if bwrap is also
available. The cache protection policy blocks writes to the shared cache.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ rm -rf _build
  $ dune build --sandbox-actions result \
  >   && cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

Explicit `--sandbox-actions-backend=bwrap` runs the action through bwrap.
The cache protection policy blocks writes to the shared cache.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ rm -rf _build
  $ dune build --sandbox-actions --sandbox-actions-backend=bwrap result \
  >   && cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

Both backends stacked also block cache writes.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ rm -rf _build
  $ dune build --sandbox-actions \
  >   --sandbox-actions-backend=bwrap \
  >   --sandbox-actions-backend=landlock \
  >   result && cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

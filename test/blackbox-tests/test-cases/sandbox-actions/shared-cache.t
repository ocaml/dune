Before the Landlock cache policy is installed, `--sandbox-actions` still uses
bwrap when it is available, so the existing bwrap shared-cache bind prevents
the worker from writing to the launching dune's shared cache.

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

Normal actions can still write there.

  $ dune build result
  $ cat _build/default/result
  wrote
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present
  present

Sandboxed actions are blocked from writing there by bwrap.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ dune build --sandbox-actions result
  $ cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

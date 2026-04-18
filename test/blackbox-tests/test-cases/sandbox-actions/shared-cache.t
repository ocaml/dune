`--sandbox-actions` prevents the worker from writing to the launching dune's
shared cache.

  $ make_dune_project 3.23
  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db"
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
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

Sandboxed actions are blocked from writing there.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ dune build --sandbox-actions result
  $ cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

If the shared cache path does not exist yet, dune creates it and still protects
it from the worker.

  $ export DUNE_CACHE_ROOT=$PWD/fresh-cache-root
  $ test -e "$DUNE_CACHE_ROOT/db" && echo present || echo missing
  missing
  $ dune build --sandbox-actions result
  $ cat _build/default/result
  blocked
  $ test -d "$DUNE_CACHE_ROOT/db" && echo present || echo missing
  present
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

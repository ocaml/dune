`dune internal with-landlock` runs commands with the shared cache read-only.

  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db" writable
  $ echo cache > "$DUNE_CACHE_ROOT/db/item"
  $ cat > check-cache-policy.sh <<'EOF'
  > try_touch () {
  >   if touch "$1" 2>/dev/null; then echo "$2"; else echo "$3"; fi
  > }
  > cat "$DUNE_CACHE_ROOT/db/item"
  > try_touch "$DUNE_CACHE_ROOT/db/new" cache-wrote cache-blocked
  > try_touch writable/outside outside-wrote outside-blocked
  > EOF
  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   dune internal with-landlock -- sh check-cache-policy.sh
  > else
  >   echo cache
  >   echo cache-blocked
  >   echo outside-wrote
  > fi
  cache
  cache-blocked
  outside-wrote
  $ test -e "$DUNE_CACHE_ROOT/db/new" && echo present || echo missing
  missing

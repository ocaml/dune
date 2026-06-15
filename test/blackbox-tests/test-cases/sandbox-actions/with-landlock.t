`dune internal with-landlock` runs commands with the shared cache read-only.

  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db" writable
  $ echo cache > "$DUNE_CACHE_ROOT/db/item"
  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   dune internal with-landlock -- sh -c 'cat "$DUNE_CACHE_ROOT/db/item"; if touch "$DUNE_CACHE_ROOT/db/new" 2>/dev/null; then echo cache-wrote; else echo cache-blocked; fi; if touch writable/outside 2>/dev/null; then echo outside-wrote; else echo outside-blocked; fi'
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

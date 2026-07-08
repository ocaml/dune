`dune internal with-landlock --deny-write PATH` blocks writes within that subtree.

  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db" writable
  $ echo cache > "$DUNE_CACHE_ROOT/db/item"
  $ dune internal with-landlock --deny-write "$DUNE_CACHE_ROOT/db" -- sh -c 'cat "$DUNE_CACHE_ROOT/db/item"; if touch "$DUNE_CACHE_ROOT/db/new" 2>/dev/null; then echo cache-wrote; else echo cache-blocked; fi; if touch writable/outside 2>/dev/null; then echo outside-wrote; else echo outside-blocked; fi'
  cache
  cache-blocked
  outside-wrote
  $ test -e "$DUNE_CACHE_ROOT/db/new" && echo present || echo missing
  missing

Rename/link out of a denied tree and truncation of a denied file are blocked.
These are the ABI 2/3 rights that keep a protected file from being moved or
emptied through a non-write operation.

  $ rm -f writable/item writable/item-link
  $ dune internal with-landlock --deny-write "$DUNE_CACHE_ROOT/db" -- sh -c 'if mv "$DUNE_CACHE_ROOT/db/item" writable/item 2>/dev/null; then echo moved; else echo move-blocked; fi; if ln "$DUNE_CACHE_ROOT/db/item" writable/item-link 2>/dev/null; then echo linked; else echo link-blocked; fi; if truncate -s 0 "$DUNE_CACHE_ROOT/db/item" 2>/dev/null; then echo truncated; else echo truncate-blocked; fi'
  move-blocked
  link-blocked
  truncate-blocked
  $ test -e "$DUNE_CACHE_ROOT/db/item" && echo present || echo missing
  present
  $ test -e writable/item && echo present || echo missing
  missing
  $ test -e writable/item-link && echo present || echo missing
  missing
  $ cat "$DUNE_CACHE_ROOT/db/item"
  cache

Symlink aliases of denied paths are not granted write rules while enumerating
siblings of protected ancestors.

  $ rm -f alias
  $ ln -s cache-root alias
  $ dune internal with-landlock --deny-write "$DUNE_CACHE_ROOT/db" -- sh -c 'if touch alias/db/through-alias 2>/dev/null; then echo alias-wrote; else echo alias-blocked; fi'
  alias-blocked
  $ test -e "$DUNE_CACHE_ROOT/db/through-alias" && echo present || echo missing
  missing

The ancestor directories of a denied path are not granted directory write
rights, so creating new entries directly in those ancestors is denied too.

  $ rm -f ancestor-new
  $ dune internal with-landlock --deny-write "$DUNE_CACHE_ROOT/db" -- sh -c 'if touch ancestor-new 2>/dev/null; then echo ancestor-wrote; else echo ancestor-blocked; fi'
  ancestor-blocked
  $ test -e ancestor-new && echo present || echo missing
  missing

Without any flags, `dune internal with-landlock` is a no-op.

  $ rm -f "$DUNE_CACHE_ROOT/db/no-flag-write"
  $ dune internal with-landlock -- sh -c 'touch "$DUNE_CACHE_ROOT/db/no-flag-write" && echo wrote'
  wrote
  $ test -e "$DUNE_CACHE_ROOT/db/no-flag-write" && echo present || echo missing
  present

Allow flags are punch-holes in denied subtrees. Without a corresponding deny
flag they do not turn the policy into a deny-everything-else allow-list.

  $ mkdir -p "$DUNE_CACHE_ROOT/db/allowed"
  $ rm -f "$DUNE_CACHE_ROOT/db/allowed/punch-hole" "$DUNE_CACHE_ROOT/db/blocked"
  $ dune internal with-landlock \
  >   --deny-write "$DUNE_CACHE_ROOT/db" \
  >   --allow-write "$DUNE_CACHE_ROOT/db/allowed" \
  >   -- sh -c 'if touch "$DUNE_CACHE_ROOT/db/allowed/punch-hole" 2>/dev/null; then echo allow-wrote; else echo allow-blocked; fi; if touch "$DUNE_CACHE_ROOT/db/blocked" 2>/dev/null; then echo deny-wrote; else echo deny-blocked; fi'
  allow-wrote
  deny-blocked
  $ test -e "$DUNE_CACHE_ROOT/db/allowed/punch-hole" && echo present || echo missing
  present
  $ test -e "$DUNE_CACHE_ROOT/db/blocked" && echo present || echo missing
  missing

  $ rm -f "$DUNE_CACHE_ROOT/db/allow-only-write"
  $ dune internal with-landlock --allow-write writable -- sh -c 'touch "$DUNE_CACHE_ROOT/db/allow-only-write" && echo wrote'
  wrote
  $ test -e "$DUNE_CACHE_ROOT/db/allow-only-write" && echo present || echo missing
  present

Allow paths must be strictly inside a denied path. An allow path that covers a
deny path is rejected instead of silently nullifying the deny.

  $ dune internal with-landlock --deny-write "$DUNE_CACHE_ROOT/db" --allow-write "$DUNE_CACHE_ROOT" -- true 2>&1 | sed "s#$PWD#PWD#g"
  Error: --allow-write path
  PWD/cache-root
  must be strictly inside a --deny-write path
  [1]

An allow path is also rejected if it covers another denied path.

  $ mkdir -p "$DUNE_CACHE_ROOT/db/secret"
  $ dune internal with-landlock \
  >   --deny-write "$DUNE_CACHE_ROOT/db" \
  >   --deny-write "$DUNE_CACHE_ROOT/db/secret" \
  >   --allow-write "$DUNE_CACHE_ROOT/db/secret" \
  >   -- true 2>&1 | sed "s#$PWD#PWD#g"
  Error: --allow-write path
  PWD/cache-root/db/secret
  must not cover --deny-write path
  PWD/cache-root/db/secret
  [1]

Allow paths must exist when the policy is installed. Missing allow paths are
rejected.

  $ rm -rf "$DUNE_CACHE_ROOT/missing"
  $ dune internal with-landlock \
  >   --deny-write "$DUNE_CACHE_ROOT" \
  >   --allow-write "$DUNE_CACHE_ROOT/missing" \
  >   -- true 2>&1 | sed "s#$PWD#PWD#g"
  Error: failed to allow writes to
  PWD/cache-root/missing:
  No such file or directory
  [1]
  $ test -e "$DUNE_CACHE_ROOT/missing" && echo present || echo missing
  missing

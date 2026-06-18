`dune internal with-bwrap` runs commands in Dune's bubblewrap wrapper.

  $ readlink /proc/self/ns/mnt > host-ns
  $ dune internal with-bwrap -- sh -c 'echo wrapped; readlink /proc/self/ns/mnt > wrapped-ns'
  wrapped
  $ cmp -s host-ns wrapped-ns && echo same || echo different
  different

It also applies Dune's shared-cache policy.

  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db" writable
  $ echo cache > "$DUNE_CACHE_ROOT/db/item"
  $ dune internal with-bwrap -- sh -c 'cat "$DUNE_CACHE_ROOT/db/item"; if touch "$DUNE_CACHE_ROOT/db/new" 2>/dev/null; then echo cache-wrote; else echo cache-blocked; fi; if touch writable/outside 2>/dev/null; then echo outside-wrote; else echo outside-blocked; fi'
  cache
  cache-blocked
  outside-wrote
  $ test -e "$DUNE_CACHE_ROOT/db/new" && echo present || echo missing
  missing

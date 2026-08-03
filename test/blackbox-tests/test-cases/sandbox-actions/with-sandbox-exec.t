`dune internal with-sandbox-exec` runs commands in Dune's sandbox-exec wrapper.

  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db"
  $ cat > probe.sh <<'EOF'
  > echo wrapped > outside
  > if touch "$DUNE_CACHE_ROOT/db/runner-marker" 2>/dev/null; then
  >   echo wrote
  > else
  >   echo blocked
  > fi
  > EOF
  $ dune internal with-sandbox-exec -- sh probe.sh
  blocked
  $ cat outside
  wrapped
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

The canonical cache path is also protected when the configured cache root is a
symlink.

  $ mkdir -p real-cache-root/db
  $ ln -s "$PWD/real-cache-root" cache-root-link
  $ export DUNE_CACHE_ROOT=$PWD/cache-root-link
  $ dune internal with-sandbox-exec -- sh probe.sh
  blocked
  $ test -e "$PWD/real-cache-root/db/runner-marker" && echo present || echo missing
  missing

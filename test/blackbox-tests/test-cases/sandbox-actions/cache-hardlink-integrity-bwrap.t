Cache-integrity attacks through sandbox aliases are not closed by bwrap's
worker-level cache bind.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_STORAGE_MODE=hardlink
  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target dep.txt)
  >  (action (with-stdout-to %{target} (echo "original"))))
  > (rule
  >  (target result)
  >  (deps (sandbox always) dep.txt)
  >  (action
  >   (bash
  >    "chmod 777 dep.txt && echo poisoned > dep.txt && cp dep.txt %{target}")))
  > EOF

  $ check_cache_contents() {
  >   rm -rf _build
  >   dune build dep.txt 2>/dev/null
  >   cat _build/default/dep.txt
  > }

`--sandbox-actions-backend=bwrap`: bwrap's `--ro-bind` denies writes through
paths under `<cache>`, but the action writes through `_build/.sandbox/...`,
which is on the read-write `/` bind. The hardlinked inode is mutated through
the writable mount.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=hardlink \
  >   --sandbox-actions --sandbox-actions-backend=bwrap result
  $ check_cache_contents
  poisoned

With both backends stacked under hardlink sandboxing: still corrupted, since
neither blocks the write path that aliases the cache inode.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=hardlink \
  >   --sandbox-actions \
  >   --sandbox-actions-backend=bwrap \
  >   --sandbox-actions-backend=landlock result
  $ check_cache_contents
  poisoned

bwrap + symlink: chmod and write resolve through the symlink to
`_build/default/dep.txt`, which is on the read-write `/` bind. The cache
RO bind doesn't cover this path. Cache corrupted.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=symlink \
  >   --sandbox-actions --sandbox-actions-backend=bwrap result
  $ check_cache_contents
  poisoned

bwrap + landlock + symlink: still corrupted, for the same combined reasons as
the individual backends.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=symlink \
  >   --sandbox-actions \
  >   --sandbox-actions-backend=bwrap \
  >   --sandbox-actions-backend=landlock result
  $ check_cache_contents
  poisoned

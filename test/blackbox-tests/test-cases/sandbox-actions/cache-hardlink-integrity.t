Cache-integrity attack: a sandboxed action that chmods and writes a
hardlinked dependency can corrupt the shared cache entry. The cache stores
files via hardlinks (default mode). In hardlink sandboxes, the sandbox entry
shares the cache inode directly. In symlink sandboxes, the sandbox symlink
resolves to the build-dir file, which also shares the cache inode. In both
cases, the action writes through paths outside the cache directory, so bwrap's
`--ro-bind <cache>` and Landlock's `--deny-write <cache>` both miss the write.

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

Helper: clear the build dir, restore dep.txt from cache, and print its
contents. If the cache entry was corrupted in a prior step, the restored
contents will not match the original.

  $ check_cache_contents() {
  >   rm -rf _build
  >   dune build dep.txt 2>/dev/null
  >   cat _build/default/dep.txt
  > }

Baseline: no sandbox-actions. The sandboxed action's chmod+write succeeds
because the action's `dep.txt` shares an inode with the cache entry. After
clearing the build dir, the cache returns the corrupted contents.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=hardlink result
  $ check_cache_contents
  poisoned

`--sandbox-actions-backend=landlock`: same outcome. The worker-level Landlock
policy denies writes under the cache subtree by path. The write goes through
the hardlink inside the sandbox, which is allowed, so the inode is mutated.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=hardlink \
  >   --sandbox-actions --sandbox-actions-backend=landlock result
  $ check_cache_contents
  poisoned

Now repeat the four sandbox-actions configurations with `--sandbox=symlink`.
The mechanism is different from hardlink mode: chmod and write both follow the
symlink to the build-dir hardlink, which shares the cache inode. The empirical
outcome with the current worker-level policies is the same. Recorded here so
the limit of each backend is visible.

Baseline symlink: cache corrupted.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=symlink result
  $ check_cache_contents
  poisoned

landlock + symlink: Landlock evaluates rules on the resolved path. The
symlink resolves to `_build/default/dep.txt`, which is *not* in the
denied cache subtree. Open allowed, cache corrupted. Unlike hardlink
mode, this case is in principle fixable: extending the policy to also
deny writes to the build directory would catch the resolved-path open.
The current `--sandbox-actions-backend=landlock` does not enable that.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=symlink \
  >   --sandbox-actions --sandbox-actions-backend=landlock result
  $ check_cache_contents
  poisoned

The actual protection: `--sandbox=copy`. The sandbox dep is an independent
inode, so the action's chmod+write does not touch the cache entry's inode.
The action corrupts its own sandbox copy, but the cache is intact.

  $ rm -rf .cache _build
  $ dune build dep.txt
  $ dune build --sandbox=copy result
  $ check_cache_contents
  original

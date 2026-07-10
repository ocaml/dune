Verify the claim that `dune internal with-landlock` with an extended
deny-write policy (cache + build directory) blocks the cache-corruption
attack for the sandbox=symlink configuration. This is a synthetic test:
it constructs the inode chain manually rather than going through
`dune build`, so the policy can be varied without changing dune-side code.

Scenario A: deny writes to the cache only, matching the current worker-level
Landlock write policy. The attack writes through the sandbox symlink, which
resolves to `build/dep.txt`. Landlock's check fires on the resolved path, which
is not in the denied subtree, so the write proceeds and the cache is corrupted.

  $ rm -rf scratch-a
  $ mkdir -p scratch-a/cache scratch-a/build scratch-a/sandbox
  $ echo original > scratch-a/cache/dep.txt
  $ chmod 444 scratch-a/cache/dep.txt
  $ ln scratch-a/cache/dep.txt scratch-a/build/dep.txt
  $ ln -s "$PWD/scratch-a/build/dep.txt" scratch-a/sandbox/dep.txt
  $ dune internal with-landlock --deny-write "$PWD/scratch-a/cache" -- \
  >   sh -c 'chmod 777 scratch-a/sandbox/dep.txt 2>/dev/null && (echo poisoned > scratch-a/sandbox/dep.txt) 2>/dev/null && echo wrote || echo blocked'
  wrote
  $ cat scratch-a/cache/dep.txt
  poisoned

Scenario B: deny writes to both the cache and the build directory. The
resolved path of the symlink now falls in the denied build subtree, so the
destructive write returns EACCES. The chmod still succeeds (landlock does
not gate chmod) but the write itself is blocked, and the cache content
survives.

  $ rm -rf scratch-b
  $ mkdir -p scratch-b/cache scratch-b/build scratch-b/sandbox
  $ echo original > scratch-b/cache/dep.txt
  $ chmod 444 scratch-b/cache/dep.txt
  $ ln scratch-b/cache/dep.txt scratch-b/build/dep.txt
  $ ln -s "$PWD/scratch-b/build/dep.txt" scratch-b/sandbox/dep.txt
  $ dune internal with-landlock \
  >   --deny-write "$PWD/scratch-b/cache" \
  >   --deny-write "$PWD/scratch-b/build" \
  >   -- sh -c 'chmod 777 scratch-b/sandbox/dep.txt 2>/dev/null && (echo poisoned > scratch-b/sandbox/dep.txt) 2>/dev/null && echo wrote || echo blocked'
  blocked
  $ cat scratch-b/cache/dep.txt
  original

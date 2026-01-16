Test basic cache store/restore functionality in the [copy] mode.

Dune supports setting the cache directory in two ways, via the [XDG_CACHE_HOME]
variable, and via the [DUNE_CACHE_ROOT] variable. Here we test the former.

  $ export XDG_RUNTIME_DIR=$PWD/.xdg-runtime
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-storage-mode copy)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 3.5)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (deps source)
  >  (targets target1 target2)
  >  (action
  >   (progn
  >    (no-infer (with-stdout-to beacon (echo "")))
  >    (with-stdout-to target1 (cat source))
  >    (with-stdout-to target2 (cat source source)))))
  > EOF

It's a duck. It quacks. (Yes, the author of this comment didn't get it.)

  $ cat > source <<EOF
  > \_o< COIN
  > EOF

Test that after the build, the files in the build directory have the hard link
count of 1, because they are not shared with the corresponding cache entries.

Build target1 with cache tracing enabled. We expect to see both workspace-local
and shared cache misses, because we've never built target1 before.

  $ export DUNE_TRACE=cache
  $ dune build --config-file=config target1

Verify we see cache miss events for our targets in the trace:

  $ dune trace cat | jq -s 'include "dune"; cacheMissesMatching("source|target1")'
  {
    "name": "workspace_local_miss",
    "target": "_build/default/source",
    "reason": "never seen this target before"
  }
  {
    "name": "miss",
    "target": "_build/default/source",
    "reason": "not found in cache"
  }
  {
    "name": "workspace_local_miss",
    "target": "_build/default/target1",
    "reason": "never seen this target before"
  }
  {
    "name": "miss",
    "target": "_build/default/target1",
    "reason": "not found in cache"
  }

  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target1
  1
  $ dune_cmd stat hardlinks _build/default/target2
  1
  $ dune_cmd exists _build/default/beacon
  true

Test that rebuilding works. Now we expect to see only workspace-local cache
misses, because we've cleaned _build/default but not the shared cache.

  $ rm -rf _build/default
  $ dune build --config-file=config target1

Verify we see only workspace-local miss events for our targets (shared cache hits should not appear as misses):

  $ dune trace cat | jq -s 'include "dune"; cacheMissesMatching("source|target1")'
  {
    "name": "workspace_local_miss",
    "target": "_build/default/source",
    "reason": "target missing from build dir"
  }
  {
    "name": "workspace_local_miss",
    "target": "_build/default/target1",
    "reason": "target missing from build dir"
  }

  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target1
  1
  $ dune_cmd stat hardlinks _build/default/target2
  1
  $ dune_cmd exists _build/default/beacon
  false
  $ cat _build/default/source
  \_o< COIN
  $ cat _build/default/target1
  \_o< COIN
  $ cat _build/default/target2
  \_o< COIN
  \_o< COIN

Test that the zero build is indeed a zero build (nothing should be rebuilt).
No cache misses should appear in the trace.

  $ dune build --config-file=config target1

  $ dune trace cat | jq -s 'include "dune"; [ .[] | cacheMisses ] | length'
  0

Test that the cache stores all historical build results.

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune-v1 <<EOF
  > (rule
  >  (targets t1)
  >  (action (bash "echo running; echo v1 > t1")))
  > (rule
  >  (deps t1)
  >  (targets t2)
  >  (action (bash "echo running; cat t1 t1 > t2")))
  > EOF
  $ cat > dune-v2 <<EOF
  > (rule
  >  (targets t1)
  >  (action (bash "echo running; echo v2 > t1")))
  > (rule
  >  (deps t1)
  >  (targets t2)
  >  (action (bash "echo running; cat t1 t1 > t2")))
  > EOF
  $ cp dune-v1 dune
  $ dune build --config-file=config t2
  running
  running
  $ cat _build/default/t2
  v1
  v1
  $ cp dune-v2 dune
  $ dune build --config-file=config t2
  running
  running
  $ cat _build/default/t2
  v2
  v2
  $ cp dune-v1 dune
  $ dune build --config-file=config t2
  $ cat _build/default/t1
  v1
  $ cat _build/default/t2
  v1
  v1

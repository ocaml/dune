Test basic cache store/restore functionality in the default [hardlink] mode.

Dune supports setting the cache directory in two ways, via the [XDG_CACHE_HOME]
variable, and via the [DUNE_CACHE_ROOT] variable. Here we test the former.

  $ export XDG_CACHE_HOME=$(dune_cmd native-path $PWD/.xdg-cache)
  $ setup_xdg_runtime_dir

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (cache enabled)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target1 target2)
  >   (action (bash "touch beacon; cat source > target1; cat source source > target2")))
  > EOF

It's a duck. It quacks. (Yes, the author of this comment didn't get it.)

  $ cat > source <<EOF
  > \_o< COIN
  > EOF

Test that after the build, the generated targets are shared with their cache
entries. The source copy is a build-system primitive and is not shared with the
cache.

Build target1 with cache tracing enabled. We expect to see both workspace-local
and shared cache misses, because we've never built target1 before.

  $ export DUNE_TRACE=cache
  $ dune build --config-file=config target1

Verify we see cache miss events for the generated target in the trace:

  $ dune trace cat | jq -s 'include "dune"; cacheMissesMatching("target1")'
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
  $ dune trace cat | jq -s 'include "dune";
  > [ .[] | cacheMisses | select((.args.target // .args.head) == "_build/default/source") ]
  > | length'
  0

  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target1
  2
  $ dune_cmd stat hardlinks _build/default/target2
  2
  $ dune_cmd exists _build/default/beacon
  true

Test that rebuilding works. Now we expect to see only workspace-local cache
misses, because we've cleaned _build/default but not the shared cache.

  $ rm -rf _build/
  $ dune build --config-file=config target1

Verify we see only workspace-local miss events for the generated target (shared
cache hits should not appear as misses):

  $ dune trace cat | jq -s 'include "dune"; cacheMissesMatching("target1")'
  {
    "name": "workspace_local_miss",
    "target": "_build/default/target1",
    "reason": "never seen this target before"
  }
  $ dune trace cat | jq -s 'include "dune";
  > [ .[] | cacheMisses | select((.args.target // .args.head) == "_build/default/source") ]
  > | length'
  0

  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target1
  2
  $ dune_cmd stat hardlinks _build/default/target2
  2
  $ dune_cmd exists _build/default/beacon
  false
  $ cat _build/default/source
  \_o< COIN
  $ cat _build/default/target1
  \_o< COIN
  $ cat _build/default/target2
  \_o< COIN
  \_o< COIN

Shared-cache keys for rules depending on source-copy primitives are stable
across workspaces.

  $ runs="$PWD/cross-workspace-runs"
  $ mkdir ws1 ws2
  $ for dir in ws1 ws2; do
  > cat > $dir/dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  > cat > $dir/dune <<EOF
  > (rule
  >  (deps source (sandbox none))
  >  (target result)
  >  (action (bash "cat source > result; echo running >> $runs")))
  > EOF
  > printf same > $dir/source
  > done
  $ (cd ws1 && dune build --config-file=../config result)
  $ (cd ws2 && dune build --config-file=../config result)
  $ cat cross-workspace-runs
  running
  $ cat ws2/_build/default/result
  same

Rules with direct source-tree dependency facts must not use source metadata
stamps as cache keys. A same-size content edit with the original mtime restored
should still rebuild instead of restoring stale output from the shared cache.

  $ mkdir direct-source-tree-cache
  $ cd direct-source-tree-cache
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ mkdir tree
  $ printf one > tree/a
  $ touch -t 200001010000 tree/a
  $ runs="$PWD/direct-source-runs"
  $ cat > dune <<EOF
  > (rule
  >  (deps (source_tree tree) (sandbox none))
  >  (target result)
  >  (action
  >   (bash "cat tree/a > result; printf 'running:%s\n' \"\$(cat tree/a)\" >> $runs")))
  > EOF
  $ dune build --config-file=../config result
  $ cat _build/default/result
  one
  $ printf two > tree/a
  $ touch -t 200001010000 tree/a
  $ rm -rf _build
  $ dune build --config-file=../config result
  $ cat _build/default/result
  two
  $ cat direct-source-runs
  running:one
  running:two
  $ cd ..

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
  >   (targets t1)
  >   (action (bash "echo running; echo v1 > t1")))
  > (rule
  >   (deps t1)
  >   (targets t2)
  >   (action (bash "echo running; cat t1 t1 > t2")))
  > EOF
  $ cat > dune-v2 <<EOF
  > (rule
  >   (targets t1)
  >   (action (bash "echo running; echo v2 > t1")))
  > (rule
  >   (deps t1)
  >   (targets t2)
  >   (action (bash "echo running; cat t1 t1 > t2")))
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

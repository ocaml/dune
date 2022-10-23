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

Test that after the build, the files in the build directory have the hard link
count of 1, because they are not shared with the corresponding cache entries.

We expect to see both workspace-local and shared cache misses, because we've
never built [target1] before.

  $ dune build --config-file=config target1 --debug-cache=shared,workspace-local \
  >   2>&1 | grep '_build/default/source\|_build/default/target'
  Workspace-local cache miss: _build/default/source: never seen this target before
  Shared cache miss [46613c392d7e1d9e094764e41ad65596] (_build/default/source): not found in cache
  Workspace-local cache miss: _build/default/target1: never seen this target before
  Shared cache miss [ad917d574b21794a34fb1eb2c67ed0a6] (_build/default/target1): not found in cache

  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target1
  1
  $ dune_cmd stat hardlinks _build/default/target2
  1
  $ dune_cmd exists _build/default/beacon
  true

Test that rebuilding works.

Now we expect to see only workspace-local cache misses, because we've cleaned
[_build/default] but not the shared cache.

  $ rm -rf _build/default
  $ dune build --config-file=config target1 --debug-cache=shared,workspace-local \
  >   2>&1 | grep '_build/default/source\|_build/default/target'
  Workspace-local cache miss: _build/default/source: target missing from build dir
  Workspace-local cache miss: _build/default/target1: target missing from build dir

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

Test how zero the zero build is. We do not expect to see any cache misses.

  $ dune build --config-file=config target1 --debug-cache=shared,workspace-local \
  >   2>&1 | grep '_build/default/source\|_build/default/target'
  [1]

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

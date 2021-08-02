Test basic cache store/restore functionality in the default [hardlink] mode.

Dune supports setting the cache directory in two ways, via the [XDG_CACHE_HOME]
variable, and via the [DUNE_CACHE_ROOT] variable. Here we test the former.

  $ export XDG_RUNTIME_DIR=$PWD/.xdg-runtime
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache

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

Test that after the build, the files in the build directory have the hard link
counts greater than 1, because they are shared with the corresponding cache entries.

  $ dune build --config-file=config target1 --debug-cache=shared,workspace-local
  $ dune_cmd stat hardlinks _build/default/source
  3
  $ dune_cmd stat hardlinks _build/default/target1
  3
  $ dune_cmd stat hardlinks _build/default/target2
  2
  $ dune_cmd exists _build/default/beacon
  true

We expect to see both workspace-local and shared cache misses in the build log,
because we've never built [target1] before.

  $ cat _build/log | grep '_build/default/source\|_build/default/target'
  # Workspace-local cache miss: _build/default/source: never seen this target before
  # Shared cache miss [8b39c1a0b45579f8da18f42be8e6aca0] (_build/default/source): not found in cache
  # Workspace-local cache miss: _build/default/target1: never seen this target before
  # Shared cache miss [fccfd1af13c64ce19b45e2a76fb8132c] (_build/default/target1): not found in cache

Test that rebuilding works.

  $ rm -rf _build/default
  $ dune build --config-file=config target1 --debug-cache=shared,workspace-local
  $ dune_cmd stat hardlinks _build/default/source
  3
  $ dune_cmd stat hardlinks _build/default/target1
  3
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

Now we expect to see only workspace-local cache misses in the build log, because
we've cleaned [_build/default] but not the shared cache.

  $ cat _build/log | grep '_build/default/source\|_build/default/target'
  # Workspace-local cache miss: _build/default/source: target missing from build dir
  # (_build/default/source)
  # Workspace-local cache miss: _build/default/target1: target missing from build dir
  # (_build/default/target1)

Test how zero the zero build is. We do not expect to see any cache misses.

  $ dune build --config-file=config target1 --debug-cache=shared,workspace-local
  $ cat _build/log | grep '_build/default/source\|_build/default/target'
  [1]

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
          bash t1
  running
          bash t2
  running
  $ cat _build/default/t2
  v1
  v1
  $ cp dune-v2 dune
  $ dune build --config-file=config t2
          bash t1
  running
          bash t2
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

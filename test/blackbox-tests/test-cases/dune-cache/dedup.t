Test deduplication of build artifacts when using Dune cache with hard links.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$(dune_cmd native-path $PWD/.cache)

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (copy source target)))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

Here we build [target], which is a copy of [source]. After the build, the same
file will appear in the build directory twice: (i) as [_build/default/source],
because all sources are first copied to the build directory, and (ii) as
[_build/default/target], as the build result. The source copy is a build-system
primitive and is not shared with the cache, while the generated [target] is
stored in the build cache and hardlinked with its cache entry.

  $ dune build target
  $ cat _build/default/target
  \_o< COIN
  $ dune_cmd stat hardlinks _build/default/target
  2

Demonstrate that sharing of build artifacts can be abused to corrupt the cache.

  $ chmod 777 _build/default/target
  $ echo "\_o< MEOW" > _build/default/target
  $ rm -rf _build
  $ dune build target
  $ cat _build/default/target
  \_o< MEOW

Test that by using the [copy] mode we can disable the sharing.

  $ rm -rf $DUNE_CACHE_ROOT
  $ rm -rf _build
  $ dune build target --cache-storage-mode=copy
  $ cat _build/default/target
  \_o< COIN
  $ dune_cmd stat hardlinks _build/default/target
  1

In the [copy] mode, we can't corrupt the cache so easily.

  $ chmod 777 _build/default/target
  $ echo "\_o< MEOW" > _build/default/target
  $ rm -rf _build
  $ dune build target
  $ cat _build/default/target
  \_o< COIN

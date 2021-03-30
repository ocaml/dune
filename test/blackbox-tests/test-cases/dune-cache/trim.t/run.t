  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_EXIT_NO_CLIENT=1
  $ export XDG_RUNTIME_DIR=$PWD/.xdg-runtime
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (targets target_a)
  >   (action (bash "touch beacon_a; echo target_a > target_a")))
  > (rule
  >   (targets target_b)
  >   (action (bash "touch beacon_b; echo target_b > target_b")))
  > (rule
  >   (targets non-exe)
  >   (action (bash "echo content > non-exe")))
  > (rule
  >   (targets exe)
  >   (action (bash "echo content > exe; chmod +x exe")))
  > (rule
  >   (targets multi_a multi_b)
  >   (action (bash "touch beacon_multi; echo multi_a > multi_a; echo multi_b > multi_b")))
  > EOF

Function to reset build tree and cache.

  $ reset ()
  > {
  >   rm -rf _build
  >   rm -rf $XDG_CACHE_HOME/dune
  > }

Check that trimming does not crash when the cache directory does not exist.

  $ dune cache trim --size 0B
  Freed 0 bytes

Check that the digest scheme for executable and non-excutable digests hasn't
changed. If it has, make sure to increment the version of the cache. Note that
the current digests for both files match those computed by Jenga.

  $ dune build exe non-exe

  $ (cd "$PWD/.xdg-cache/dune/db/files/v4"; grep -rws . -e 'content' | sort)
  ./5e/5e5bb3a0ec0e689e19a59c3ee3d7fca8:content
  ./62/6274851067c88e9990e912be27cce386:content

Move all current entries to v3 to test trimming of old versions of cache.

  $ mkdir "$PWD/.xdg-cache/dune/db/files/v3"
  $ mkdir "$PWD/.xdg-cache/dune/db/meta/v3"
  $ mv "$PWD/.xdg-cache/dune/db/files/v4"/* "$PWD/.xdg-cache/dune/db/files/v3"
  $ mv "$PWD/.xdg-cache/dune/db/meta/v5"/* "$PWD/.xdg-cache/dune/db/meta/v3"

Build some more targets.

  $ dune build target_a target_b

Dune stores the result of rule execution in a store keyed by "rule
digests". If the way such rule digests are computed changes, we could
end up in a situation where the same hash means something different
before and after the change, which is bad. To reduce the risk, we
inject a version number into rule digests.

If you see the test below breaking, this means you changed the metadata format
or the way that digests are computed and you should increment the corresponding
version number. This number is stored in the [rule_digest_version] variable in
[build_system.ml]. You may also need to change the versioning in [layout.ml] in
the [dune_cache_storage] library and make sure that the cache trimmer treats new
and old cache entries uniformly.

  $ (cd "$PWD/.xdg-cache/dune/db/meta/v5"; grep -rws . -e 'metadata' | sort)
  ./06/061fb516fd28c9a632c573f380b8a120:((8:metadata)(5:files(8:target_a32:5637dd9730e430c7477f52d46de3909c)))
  ./50/50148ac6fcde0b35e357cbd120131dbc:((8:metadata)(5:files(8:target_b32:8a53bfae3829b48866079fa7f2d97781)))

  $ dune_cmd stat size "$PWD/.xdg-cache/dune/db/meta/v5/06/061fb516fd28c9a632c573f380b8a120"
  70

Trimming the cache at this point should not remove anything because all file
entries are still hard-linked from the build directory.

  $ dune cache trim --trimmed-size 1B
  Freed 0 bytes
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2

If we unlink a file in the build tree, then the corresponding file entry will be
trimmed.

  $ rm -f _build/default/target_a _build/default/beacon_a _build/default/beacon_b
  $ dune cache trim --trimmed-size 1B
  Freed 79 bytes
  $ dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ test -e _build/default/beacon_a
  $ ! test -e _build/default/beacon_b

  $ reset

The cache deletes oldest files first.

  $ dune build target_b
  $ dune_cmd wait-for-fs-clock-to-advance
  $ dune build target_a
The bellow rm commands also update the ctime, so we need to do it in
the same order to preserve the fact that target_b is older than
target_a:
  $ rm -f _build/default/beacon_b _build/default/target_b
  $ dune_cmd wait-for-fs-clock-to-advance
  $ rm -f _build/default/beacon_a _build/default/target_a
  $ dune cache trim --trimmed-size 1B
  Freed 79 bytes
  $ dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ ! test -e _build/default/beacon_a
  $ test -e _build/default/beacon_b

  $ reset

When a cache entry becomes unused, its ctime is modified and will determine the order of trimming.

  $ dune build target_a target_b
  $ rm -f _build/default/beacon_a _build/default/target_a
  $ dune_cmd wait-for-fs-clock-to-advance
  $ rm -f _build/default/beacon_b _build/default/target_b
  $ dune cache trim --trimmed-size 1B
  Freed 79 bytes
  $ dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ test -e _build/default/beacon_a
  $ ! test -e _build/default/beacon_b

  $ reset

Check background trimming.

  $ env -u DUNE_CACHE_EXIT_NO_CLIENT \
  >   DUNE_CACHE_TRIM_SIZE=1 \
  >   DUNE_CACHE_TRIM_PERIOD=1 \
  >   dune cache start > /dev/null 2>&1
  $ dune build target_a
  $ rm -f _build/default/target_a _build/default/beacon_a
  $ sleep 2
  $ dune build target_a
  $ test -e _build/default/beacon_a
  $ dune cache stop

  $ reset

Check garbage collection: both multi_a and multi_b must be removed as
they are part of the same rule.

  $ dune build multi_a multi_b
  $ rm -f _build/default/multi_a _build/default/multi_b
  $ dune cache trim --trimmed-size 1B
  Freed 123 bytes

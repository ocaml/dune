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

Check that trimming does not crash when the cache directory does not exist.

  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --size 0B
  Freed 0 bytes

Check that the digest scheme for executable and non-excutable digests hasn't
changed. If it has, make sure to increment the version of the cache. Note that
the current digests for both files match those computed by Jenga.

  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build exe non-exe

  $ (cd "$PWD/.xdg-cache/dune/db/files/v4"; grep -rws . -e 'content' | sort)
  ./5e/5e5bb3a0ec0e689e19a59c3ee3d7fca8:content
  ./62/6274851067c88e9990e912be27cce386:content

Move all current v4 entries to v3 to test trimming of old versions of cache.

  $ mkdir "$PWD/.xdg-cache/dune/db/files/v3"
  $ mkdir "$PWD/.xdg-cache/dune/db/meta/v3"
  $ mv "$PWD/.xdg-cache/dune/db/files/v4"/* "$PWD/.xdg-cache/dune/db/files/v3"
  $ mv "$PWD/.xdg-cache/dune/db/meta/v4"/* "$PWD/.xdg-cache/dune/db/meta/v3"

Build some more targets.

  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b

Have a look at one of the metadata files and its size. If the rule digest changes,
make sure to increment [rule_digest_version] in [build_system.ml].

  $ cat $PWD/.xdg-cache/dune/db/meta/v4/95/95be12ef67548c59c691567564f2c158
  ((8:metadata)(5:files(16:default/target_b32:8a53bfae3829b48866079fa7f2d97781)))

  $ dune_cmd stat size $PWD/.xdg-cache/dune/db/meta/v4/95/95be12ef67548c59c691567564f2c158
  79

Trimming the cache at this point should not remove anything, as all
files are still hard-linked in the build directory.

  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1B
  Freed 0 bytes
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2

If we unlink one file in the build tree, it can be reclaimed when trimming.

  $ rm -f _build/default/target_a _build/default/beacon_a _build/default/beacon_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1B
  Freed 88 bytes
  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ test -e _build/default/beacon_a
  $ ! test -e _build/default/beacon_b

Reset build tree and cache. Make sure both v3 and v4 entries are trimmed.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ rm -f _build/default/non-exe _build/default/exe
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --size 0B
  Freed 344 bytes

The cache deletes oldest files first.

  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_b
  $ sleep 1
  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1B
  Freed 88 bytes
  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ ! test -e _build/default/beacon_a
  $ test -e _build/default/beacon_b

Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18B
  Freed 176 bytes

When a cache entry becomes unused, its ctime is modified and will determine the order of trimming.

  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ rm -f _build/default/beacon_a _build/default/target_a
  $ sleep 1
  $ rm -f _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1B
  Freed 88 bytes
  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ test -e _build/default/beacon_a
  $ ! test -e _build/default/beacon_b


Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18B
  Freed 176 bytes

Check background trimming.

  $ env DUNE_CACHE=enabled DUNE_CACHE_TRIM_SIZE=1 DUNE_CACHE_TRIM_PERIOD=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache start > /dev/null 2>&1
  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ rm -f _build/default/target_a _build/default/beacon_a
  $ sleep 2
  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ test -e _build/default/beacon_a
  $ env DUNE_CACHE=enabled XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache stop

Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18B
  Freed 88 bytes

Check garbage collection: both multi_a and multi_b must be removed as
they are part of the same rule.

  $ env DUNE_CACHE=enabled DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build multi_a multi_b
  $ rm -f _build/default/multi_a _build/default/multi_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1B
  Freed 141 bytes

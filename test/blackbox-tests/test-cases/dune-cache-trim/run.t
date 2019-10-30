  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (targets target_a)
  >   (action (bash "touch beacon_a; echo target_a > target_a")))
  > (rule
  >   (targets target_b)
  >   (action (bash "touch beacon_b; echo target_b > target_b")))
  > (rule
  >   (targets multi_a multi_b)
  >   (action (bash "touch beacon_multi; echo multi_a > multi_a; echo multi_b > multi_b")))
  > EOF
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b

Trimming the cache at this point should not remove anything, as both
files are still hard-linked in the build directory.

  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1
  Freed 0 bytes
  $ ./stat.sh --format=%h _build/default/target_a
  2
  $ ./stat.sh --format=%h _build/default/target_b
  2

If we unlink one file in the build tree, it can be reclaimed when trimming.

  $ rm -f _build/default/target_a _build/default/beacon_a _build/default/beacon_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1
  Freed 9 bytes
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ ./stat.sh --format=%h _build/default/target_a
  2
  $ ./stat.sh --format=%h _build/default/target_b
  2
  $ test -e _build/default/beacon_a
  $ ! test -e _build/default/beacon_b

Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18
  Freed 18 bytes

The cache deletes oldest files first.

  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_b
  $ sleep 1
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1
  Freed 9 bytes
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ ./stat.sh --format=%h _build/default/target_a
  2
  $ ./stat.sh --format=%h _build/default/target_b
  2
  $ ! test -e _build/default/beacon_a
  $ test -e _build/default/beacon_b

Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18
  Freed 18 bytes

When a file is pulled from the cache, its mtime is touched so it's deleted last.

  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_b
  $ sleep 1
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ rm -f _build/default/target_b
  $ sleep 1
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1
  Freed 9 bytes
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a target_b
  $ ./stat.sh --format=%h _build/default/target_a
  2
  $ ./stat.sh --format=%h _build/default/target_b
  2
  $ test -e _build/default/beacon_a
  $ ! test -e _build/default/beacon_b


Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18
  Freed 18 bytes

Check background trimming.

  $ env DUNE_CACHE=1 DUNE_CACHE_TRIM_SIZE=1 DUNE_CACHE_TRIM_PERIOD=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache start > /dev/null
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ rm -f _build/default/target_a _build/default/beacon_a
  $ sleep 2
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target_a
  $ test -e _build/default/beacon_a
  $ env DUNE_CACHE=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache stop

Reset build tree and cache.

  $ rm -f _build/default/beacon_a _build/default/target_a _build/default/beacon_b _build/default/target_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 18
  Freed 9 bytes

Check garbage collection: both multi_a and multi_b must be removed as
they are part of the same rule.

  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build multi_a multi_b
  $ rm -f _build/default/multi_a _build/default/multi_b
  $ XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune cache trim --trimmed-size 1
  Freed 16 bytes

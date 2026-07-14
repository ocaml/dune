Requesting to build a single package should not build unrelated things:

  $ make_lockdir

  $ make_dune_project 3.25

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF

  $ pkg() {
  > make_lockpkg $1 <<EOF
  > (build (run echo building $1))
  > (version dev)
  > EOF
  > }

These two packages are independent:

  $ pkg foo
  $ pkg bar

We should only see the result of building "foo"

  $ build_pkg foo
  building foo

We should only see the result of building "bar"

  $ build_pkg bar
  building bar

Package build commands are not restricted by the shared-cache Landlock policy.

  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ mkdir -p "$DUNE_CACHE_ROOT/db"
  $ make_lockpkg cache-writer <<EOF
  > (build (run touch $DUNE_CACHE_ROOT/db/package-marker))
  > (version dev)
  > EOF
  $ build_pkg cache-writer
  $ test -e "$DUNE_CACHE_ROOT/db/package-marker" && echo wrote
  wrote

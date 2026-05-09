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

Lock inputs use build-directory snapshots and are not content-digested in the
source tree. Editing them must still rebuild the affected package.

  $ make_lockpkg foo <<EOF
  > (build (run echo rebuilt foo))
  > (version dev)
  > EOF
  $ build_pkg foo
  rebuilt foo
  $ cmp dune.lock/foo.pkg _build/.source-copies/dune.lock/foo.pkg
  $ dune internal digest-db dump 2>&1 | grep 'In_source_tree'
  [1]

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

Editing a lock input must rebuild the affected package.

  $ make_lockpkg foo <<EOF
  > (build (run echo rebuilt foo))
  > (version dev)
  > EOF
  $ build_pkg foo
  rebuilt foo

Lock inputs are read as configuration during rule generation. Their contents
are captured in actions without content-digesting source files.

  $ lock_copy=_build/_private/default/.lock/dune.lock
  $ cmp dune.lock/foo.pkg "$lock_copy/foo.pkg"
  $ dune internal digest-db dump 2>&1 | grep 'In_source_tree'
  [1]

Lock inputs preserve binary contents and executable bits, including changes
that leave the bytes unchanged. Removed inputs must not survive in the copy.

  $ mkdir -p dune.lock/foo.files/sub
  $ printf 'payload\000tail\n' > dune.lock/foo.files/sub/data
  $ printf '#!/bin/sh\nexit 0\n' > dune.lock/foo.files/script
  $ chmod +x dune.lock/foo.files/script
  $ dune build "$lock_copy"
  $ cmp dune.lock/foo.files/sub/data "$lock_copy/foo.files/sub/data"
  $ cmp dune.lock/foo.files/script "$lock_copy/foo.files/script"
  $ test -x "$lock_copy/foo.files/script"
  $ chmod -x dune.lock/foo.files/script
  $ dune build "$lock_copy"
  $ test ! -x "$lock_copy/foo.files/script"
  $ rm dune.lock/foo.files/sub/data
  $ dune build "$lock_copy"
  $ test ! -e "$lock_copy/foo.files/sub/data"

  $ dune rules "$lock_copy" > .#lock-rules
  $ if grep -q '(copy' .#lock-rules; then echo copy; else echo contents; fi
  contents

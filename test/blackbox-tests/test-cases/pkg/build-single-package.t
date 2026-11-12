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

Binary lock payloads reach the installed package byte-for-byte.

  $ mkdir -p dune.lock/payload.files/sub
  $ printf 'binary\000contents\n' > dune.lock/payload.files/sub/data
  $ cat > dune.lock/payload.files/tool <<'EOF'
  > #!/bin/sh
  > echo payload-ok
  > EOF
  $ chmod +x dune.lock/payload.files/tool
  $ make_lockpkg payload <<'EOF'
  > (version dev)
  > (build (run ./tool))
  > (install (run cp sub/data %{pkg-self:share}/data))
  > EOF
  $ build_pkg payload
  payload-ok
  $ payload_dir=$(get_build_pkg_dir payload)
  $ cmp dune.lock/payload.files/sub/data "$payload_dir/target/share/payload/data"
  $ build_pkg payload

Changing bytes after the NUL must invalidate the package without changing
its metadata or the payload size.

  $ printf 'binary\000modified\n' > dune.lock/payload.files/sub/data
  $ build_pkg payload
  payload-ok
  $ payload_dir=$(get_build_pkg_dir payload)
  $ cmp dune.lock/payload.files/sub/data "$payload_dir/target/share/payload/data"

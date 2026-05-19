Documenting that sources with file symlinks are re-fetched every time. The
menhir package is an example of such a package in the opam-repository. See
`./fetch-cache.t` for a normal package test.

The fetch rules are always considered safe to cache, but we make it explicit
here. We set a custom directory for the shared cache and enable cache tracing.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$(pwd)/dune-cache
  $ export DUNE_TRACE=+cache

Set up a project that depends on a package that is being downloaded. Note that
the package being downloaded has a symlink.

  $ make_lockdir
  $ echo "Contents" > tar-contents
  $ CONTENT_CHECKSUM=$(md5sum tar-contents | cut -f1 -d' ')
  $ ln -s tar-contents tar-symlink
  $ tar cf test.tar tar-contents tar-symlink
  $ echo test.tar > fake-curls
  $ SRC_PORT=1
  $ SRC_CHECKSUM=$(md5sum test.tar | cut -f1 -d' ')
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url http://localhost:$SRC_PORT)
  >   (checksum md5=$SRC_CHECKSUM)))
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package (name my) (depends test) (allow_empty))
  > EOF

The first build should succeed, fetching the source, populating the cache and
disabling the download of the source a second time.

  $ build_pkg test

We see cache store events for our targets in the trace:

  $ dune trace cat | jq -s '[.[] | select(.args.message == "cache store target creation errors") ] | length'
  1

Cleaning the project to force rebuilding. This triggers an attempt to
re-download the source, since it contains a symlink and wasn't cached:

  $ dune clean
  $ export DUNE_CACHE=enabled
  $ build_pkg test
  File "dune.lock/test.pkg", line 4, characters 7-25:
  4 |   (url http://localhost:1)
             ^^^^^^^^^^^^^^^^^^
  Error: Download failed with code 404
         
  [1]


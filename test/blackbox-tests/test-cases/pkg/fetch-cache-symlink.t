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
  $ make_fetch_cache_project tar-contents tar-symlink

The first build should succeed, fetching the source, populating the cache and
disabling the download of the source a second time.

  $ build_pkg test

We see no cache store errors and the cache store events for the fetched dir,
pkg-$DIGEST/source and pkg-$DIGEST/target in the trace:

  $ dune trace cat | jq -s '[.[] | select(.args.message == "cache store target creation errors") ] | length'
  0
  $ dune trace cat | jq -s '[.[] | select(.args.message == "cache store success") ] | length'
  3

Cleaning the project to force rebuilding. This no longer triggers an attempt to
re-download the source after the change to turn symlinks in the fetched source
into hardlinks:

  $ dune clean
  $ export DUNE_CACHE=enabled
  $ build_pkg test

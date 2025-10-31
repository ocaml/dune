Testing that files are only fetched once.

  $ . ./helpers.sh

No need to set DUNE_CACHE (enabled by default) as the
fetch rules are always considered safe to cache, but we'll set a custom
directory for the shared cache.

  $ export DUNE_CACHE_ROOT=$(pwd)/dune-cache
  $ unset DUNE_CACHE

Set up a project that depends on a package that is being downloaded

  $ make_lockdir
  $ echo "Contents" > tar-contents
  $ CONTENT_CHECKSUM=$(md5sum tar-contents | cut -f1 -d' ')
  $ tar cf test.tar tar-contents
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

Make sure that the file that was fetched is in the cache:

  $ find $DUNE_CACHE_ROOT/files -type f -exec md5sum {} \; | grep --quiet $CONTENT_CHECKSUM

Cleaning the project to force rebuilding. If we attempt to build without the
cache, it will fail, as the source is 404 now:

  $ dune clean
  $ export DUNE_CACHE=disabled
  $ build_pkg test
  File ".dune-solution-cache/test.pkg", line 4, characters 7-25:
  4 |   (url http://localhost:1)
             ^^^^^^^^^^^^^^^^^^
  Error: Download failed with code 404
         
  [1]

However when enabling the cache again, the file that was fetched in the first
build should be retrieved from the cache and the build succeed:

  $ dune clean
  $ export DUNE_CACHE=enabled
  $ build_pkg test

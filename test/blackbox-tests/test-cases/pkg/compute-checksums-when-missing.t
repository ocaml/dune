Test that dune will add checksums to lockfiles when the package has a source
archive but no checksum. This test uses an http server to serve packages to
test checksum generation, since we only generate checksums for packages
downloaded from non-local sources.
  $ . ./helpers.sh
  $ mkrepo

A file that will comprise the package source:

  $ echo "Hello, World!" > foo.txt
  $ echo foo.txt > fake-curls
  $ PORT=1

  $ mkpkg foo <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ solve foo
  Package "foo" has source archive which lacks a checksum.
  The source archive will be downloaded from: http://0.0.0.0:1
  Dune will compute its own checksum for this source archive.
  Solution for dune.lock:
  - foo.0.0.1

Replace the path in the lockfile as it would otherwise include the sandbox
path.
  $ cat ${default_lock_dir}/foo.pkg
  (version 0.0.1)
  
  (source
   (fetch
    (url http://0.0.0.0:1)
    (checksum md5=bea8252ff4e80f41719ea13cdf007273)))
  
  (dev)

Now make sure we can gracefully handle the case when the archive is missing.
Recreate the foo package with a fake port number to signal that the file will
404:

  $ PORT=9000
  $ mkpkg foo <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ solve foo 2>&1
  Package "foo" has source archive which lacks a checksum.
  The source archive will be downloaded from: http://0.0.0.0:9000
  Dune will compute its own checksum for this source archive.
  Warning: download failed with code 404
  Solution for dune.lock:
  - foo.0.0.1
  $ cat ${default_lock_dir}/foo.pkg
  (version 0.0.1)
  
  (source
   (fetch
    (url http://0.0.0.0:9000)))
  
  (dev)

Check that no checksum is computed for a local source file:

  $ mkpkg foo <<EOF
  > url {
  >  src: "$PWD/foo.txt"
  > }
  > EOF
  $ solve foo 2>&1
  Solution for dune.lock:
  - foo.0.0.1

Check that no checksum is computed for a local source directory:

  $ mkdir src
  $ mkpkg foo <<EOF
  > url {
  >  src: "$PWD/src"
  > }
  > EOF
  $ solve foo 2>&1
  Solution for dune.lock:
  - foo.0.0.1


Create 3 packages that all share the same source url with no checksum. Dune
will need to download each package's source archive to compute their hashes.
Test that dune only downloads the file a single time since the source url is
identical among the packages. The fact that the download only occurs once is
asserted by the fact that the webserver will only serve the file a single time.

  $ echo foo.txt >> fake-curls
  $ PORT=2

  $ mkpkg foo <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ mkpkg bar <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ mkpkg baz <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ solve foo bar baz
  Package "bar" has source archive which lacks a checksum.
  The source archive will be downloaded from: http://0.0.0.0:2
  Dune will compute its own checksum for this source archive.
  Package "baz" has source archive which lacks a checksum.
  The source archive will be downloaded from: http://0.0.0.0:2
  Dune will compute its own checksum for this source archive.
  Package "foo" has source archive which lacks a checksum.
  The source archive will be downloaded from: http://0.0.0.0:2
  Dune will compute its own checksum for this source archive.
  Solution for dune.lock:
  - bar.0.0.1
  - baz.0.0.1
  - foo.0.0.1

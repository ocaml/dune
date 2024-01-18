Test that dune will add checksums to lockfiles when the package has a source
archive but no checksum. This test uses an http server to serve packages to
test checksum generation, since we only generate checksums for packages
downloaded from non-local sources.
  $ . ./helpers.sh
  $ mkrepo

  $ strip_transient() {
  >   sed -e "s#$PWD#<pwd>#" | \
  >   # strip the entire address as the port number may also appear in the hash
  >   sed -e "s#http://.*$PORT#<addr>#"
  > }

A file that will comprise the package source:
  $ echo "Hello, World!" > foo.txt

Run the server in the background:
  $ webserver_oneshot --content-file foo.txt --port-file port.txt &
  $ until test -f port.txt; do sleep 0.1; done
  $ PORT=$(cat port.txt)

  $ mkpkg foo <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ solve foo | strip_transient
  Solution for dune.lock:
  - foo.0.0.1
  Package "foo" has source archive which lacks a checksum.
  The source archive will be downloaded from: <addr>
  Dune will compute its own checksum for this source archive.

  $ wait

Replace the path in the lockfile as it would otherwise include the sandbox
path.
  $ cat dune.lock/foo.pkg | strip_transient
  (version 0.0.1)
  
  (source
   (fetch
    (url <addr>)
    (checksum md5=bea8252ff4e80f41719ea13cdf007273)))
  
  (dev)

Now make sure we can gracefully handle the case when the archive is missing.

  $ rm port.txt
  $ webserver_oneshot --content-file foo.txt --port-file port.txt --simulate-not-found &
  $ until test -f port.txt; do sleep 0.1; done
  $ PORT=$(cat port.txt)

Recreate the foo package as the port number will have changed:
  $ mkpkg foo <<EOF
  > url {
  >  src: "http://0.0.0.0:$PORT"
  > }
  > EOF

  $ solve foo 2>&1 | strip_transient
  Package "foo" has source archive which lacks a checksum.
  The source archive will be downloaded from: <addr>
  Dune will compute its own checksum for this source archive.
  Warning: download failed with code 404
  Solution for dune.lock:
  - foo.0.0.1
  $ cat dune.lock/foo.pkg | strip_transient
  (version 0.0.1)
  
  (source
   (fetch
    (url <addr>)))
  
  (dev)

  $ wait

Check that no checksum is computed for a local source file:

  $ mkpkg foo <<EOF
  > url {
  >  src: "$PWD/foo.txt"
  > }
  > EOF
  $ solve foo 2>&1 | strip_transient
  Solution for dune.lock:
  - foo.0.0.1

Check that no checksum is computed for a local source directory:

  $ mkdir src
  $ mkpkg foo <<EOF
  > url {
  >  src: "$PWD/src"
  > }
  > EOF
  $ solve foo 2>&1 | strip_transient
  Solution for dune.lock:
  - foo.0.0.1


Create 3 packages that all share the same source url with no checksum. Dune
will need to download each package's source archive to compute their hashes.
Test that dune only downloads the file a single time since the source url is
identical among the packgaes. The fact that the download only occurs once is
asserted by the fact that the webserver will only serve the file a single time.
  $ webserver_oneshot --content-file foo.txt --port-file port.txt &
  $ until test -f port.txt; do sleep 0.1; done
  $ PORT=$(cat port.txt)

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

  $ solve foo bar baz | strip_transient
  Solution for dune.lock:
  - bar.0.0.1
  - baz.0.0.1
  - foo.0.0.1
  Package "bar" has source archive which lacks a checksum.
  The source archive will be downloaded from: <addr>
  Dune will compute its own checksum for this source archive.
  Package "baz" has source archive which lacks a checksum.
  The source archive will be downloaded from: <addr>
  Dune will compute its own checksum for this source archive.
  Package "foo" has source archive which lacks a checksum.
  The source archive will be downloaded from: <addr>
  Dune will compute its own checksum for this source archive.

  $ wait


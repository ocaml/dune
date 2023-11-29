This test shows a bug where the directory structure of a package will differ when it is
copied vs when it is fetched.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo <<'EOF'
  > build: [
  >   [ "dune" "build" "-p" name "-j" jobs ]
  > ]
  > EOF

The sources of the package will live here, and we will make sure to ignore them in the
test. Only allowing them to be fetched or pointed to.

  $ mkdir some_sources
  $ cat > dune <<EOF
  > (dirs :standard \ some_sources)
  > EOF
  $ cat > some_sources/dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cat > some_sources/foo.ml
  $ cat > some_sources/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1
  $ cat >>dune.lock/foo.pkg<<EOF
  > (source (copy $PWD/some_sources))
  > EOF

Building the package:

  $ build_pkg foo

We can inspect the built targets and see that the library has been built correctly:

  $ [ -d _build/_private/default/.pkg/foo/target/lib/foo ]

Now we will modify the copy rule to use a fetch instead:

  $ sed -i '$ d' dune.lock/foo.pkg
  $ cat >>dune.lock/foo.pkg<<EOF
  > (source (fetch (url "http://localhost:8000")))
  > EOF

We compress the sources to simulate a tarball:

  $ tar -czf some_sources.tar.gz some_sources

We make a web server to serve this tarball:

  $ cat > serve_tarball.sh <<'EOF'
  > #!/bin/sh
  > tarball="some_sources.tar.gz"
  > port=8000
  > 
  > while true; do echo -ne "HTTP/1.1 200 OK\r\nContent-Length: $(wc -c < "$tarball")\r\n\r\n"; cat "$tarball"; done | nc -l $port
  > EOF

  $ chmod +x serve_tarball.sh
  $ ./serve_tarball.sh 2>&1 > /dev/null &


Building the package again:

  $ build_pkg foo

Killing the server
  $ kill %1      

We can inspect the built targets and see that the library has been built incorrectly:

  $ [ -d _build/_private/default/.pkg/foo/target/lib/foo ]
  [1]


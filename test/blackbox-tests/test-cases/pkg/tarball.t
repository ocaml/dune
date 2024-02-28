Demonstrate that we should support tarballs with and without a root directory

  $ . ./helpers.sh

  $ mkdir _source/
  $ touch _source/foo

  $ tar -czf tarball1.tar.gz -C _source foo
  $ tar -czf tarball2.tar.gz _source/foo

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ make_lockdir

CR-rgrinberg: no idea why, but curl doesn't like it when we run the server
twice in one test (even on different ports)

  $ webserver_oneshot  --port-file port.txt \
  >   --content-file tarball1.tar.gz \
  >   --content-file tarball2.tar.gz &
  $ until test -f port.txt; do sleep 0.1; done
  $ port=$(cat port.txt)

  $ runtest() {
  > make_lockpkg foo <<EOF
  > (version 0.1.0)
  > (source (fetch (url http://0.0.0.0:$port)))
  > (build (run sh -c "find . | sort"))
  > EOF
  > build_pkg foo
  > rm -rf _build
  > }
  $ runtest tarball1.tar.gz
  .
  ./foo
  $ runtest tarball2.tar.gz
  .
  ./foo

  $ wait

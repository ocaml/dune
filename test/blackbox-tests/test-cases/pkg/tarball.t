Demonstrate that we should support tarballs with and without a root directory

  $ mkdir _source/
  $ touch _source/foo

  $ tar -czf tarball1.tar.gz -C _source foo
  $ tar -czf tarball2.tar.gz _source/foo

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ make_lockdir

  $ echo tarball1.tar.gz > fake-curls
  $ echo tarball2.tar.gz >> fake-curls

  $ runtest() {
  > make_lockpkg foo <<EOF
  > (version 0.1.0)
  > (source (fetch (url http://0.0.0.0:$1)))
  > (build (run sh -c "find . | sort"))
  > EOF
  > build_pkg foo
  > rm -rf _build
  > }
  $ runtest 1
  .
  ./foo
  $ runtest 2
  .
  ./foo

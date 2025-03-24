Test that dune can fetch local sources.

  $ . ./helpers.sh
  $ mkrepo

Make a local source archive:
  $ mkdir src
  $ echo hello > src/a.txt
  $ echo world > src/b.txt
  $ tar -czf src.tar.gz src

Build a package that uses the archive as its source:
  $ mkpkg foo <<EOF
  > url {
  >  src: "$PWD/src.tar.gz"
  > }
  > EOF
  $ add_mock_repo_if_needed
  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1
  $ build_pkg foo
  $ cat _build/_private/default/.pkg/foo/source/*
  hello
  world

  $ dune clean

Build a package that uses the src directory as its source:
  $ mkpkg foo <<EOF
  > url {
  >  src: "$PWD/src"
  > }
  > EOF
  $ add_mock_repo_if_needed
  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1
  $ build_pkg foo
  $ cat _build/_private/default/.pkg/foo/source/*
  hello
  world

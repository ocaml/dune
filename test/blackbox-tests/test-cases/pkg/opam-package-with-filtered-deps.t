Demonstrate the translation of filtered dependencies

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg pkg-post <<EOF
  > EOF
  $ mkpkg pkg-dev <<EOF
  > EOF
  $ mkpkg pkg-build <<EOF
  > EOF
  $ mkpkg pkg-dev <<EOF
  > EOF
  $ mkpkg pkg-test <<EOF
  > EOF

  $ mkpkg bar <<EOF
  > depends: [
  >   "pkg-post" {post}
  >   "pkg-dev" {dev}
  >   "pkg-test" {test}
  >   "pkg-doc" {doc}
  >   "pkg-build" {build}
  > ]
  > EOF

  $ solve bar 2>/dev/null

  $ cat dune.lock/bar.pkg
  (version 0.0.1)
  
  (depends pkg-build)

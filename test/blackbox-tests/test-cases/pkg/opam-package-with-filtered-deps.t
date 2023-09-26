Demonstrate the translation of filtered dependencies

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg pkg-post <<EOF
  > opam-version: "2.0"
  > EOF
  $ mkpkg pkg-dev <<EOF
  > opam-version: "2.0"
  > EOF
  $ mkpkg pkg-build <<EOF
  > opam-version: "2.0"
  > EOF
  $ mkpkg pkg-dev <<EOF
  > opam-version: "2.0"
  > EOF
  $ mkpkg pkg-test <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg bar <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "pkg-post" {post}
  >   "pkg-dev" {dev}
  >   "pkg-test" {test}
  >   "pkg-doc" {doc}
  >   "pkg-build" {build}
  > ]
  > EOF

  $ solve_project 2>/dev/null <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends bar))
  > EOF

  $ cat dune.lock/bar.pkg
  (version 0.0.1)
  
  (deps pkg-post pkg-dev pkg-test pkg-doc pkg-build)

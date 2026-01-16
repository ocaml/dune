Demonstrate the translation of filtered dependencies

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
  Solution for dune.lock:
  - bar.0.0.1
  - pkg-build.0.0.1

  $ cat ${default_lock_dir}/bar.0.0.1.pkg
  (version 0.0.1)
  
  (depends
   (all_platforms (pkg-build)))

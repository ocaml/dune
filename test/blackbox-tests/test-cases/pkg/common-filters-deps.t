Demonstrate that local dependencies that are marked as {with-test} can be
included.

  $ mkrepo

  $ mkpkg post <<EOF
  > EOF
  $ mkpkg build <<EOF
  > EOF
  $ mkpkg dev <<EOF
  > EOF
  $ mkpkg test <<EOF
  > EOF
  $ mkpkg doc <<EOF
  > EOF
  $ mkpkg dev-setup <<EOF
  > EOF

Like with-test, with-dev-setup is enabled for local packages, so its
dependencies are included in the solution. with-doc is disabled.

Dune will also not include dependencies marked `post` in the lock directory.

  $ solve "(test :with-test) (doc :with-doc) (dev-setup :with-dev-setup) (dev :with-dev) (build :build) (post :post)"
  Solution for dune.lock:
  - build.0.0.1
  - dev-setup.0.0.1
  - test.0.0.1

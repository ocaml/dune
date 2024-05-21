Demonstrate that local dependencies that are marked as {with-test} can be
included.

  $ . ./helpers.sh
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

Note that dune solves packages with with-doc and with-dev-setup always set to false, so
documentation-only deps are omitted from the solution.
  $ solve "(test :with-test) (doc :with-doc) (dev-setup :with-dev-setup) (dev :with-dev) (build :build) (post :post)"
  Solution for dune.lock:
  - build.0.0.1
  - test.0.0.1

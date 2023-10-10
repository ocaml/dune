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

  $ solve "(test :with-test) (doc :with-doc) (dev :with-dev) (build :build) (post :post)"
  Solution for dune.lock:
  build.0.0.1
  doc.0.0.1
  test.0.0.1
  

Test that we run the build command

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<EOF
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)))
  > EOF

  $ dune build .pkg/test/target/bin/foo

  $ find _build/default/.pkg/test | sort
  _build/default/.pkg/test
  _build/default/.pkg/test/target
  _build/default/.pkg/test/target/bin
  _build/default/.pkg/test/target/bin/foo
  _build/default/.pkg/test/target/cookie

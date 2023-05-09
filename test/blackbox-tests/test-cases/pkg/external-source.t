Test that can fetch the sources from an external dir

  $ mkdir foo
  $ echo "y" > foo/x

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<EOF
  > (source (copy $PWD/foo))
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run cp x %{prefix}/bin/x )))
  > EOF

  $ dune build .pkg/test/target/bin/x

  $ find _build/default/.pkg/test | sort
  _build/default/.pkg/test
  _build/default/.pkg/test/source
  _build/default/.pkg/test/source/x
  _build/default/.pkg/test/target
  _build/default/.pkg/test/target/bin
  _build/default/.pkg/test/target/bin/x
  _build/default/.pkg/test/target/cookie

Test that can fetch the sources from an external dir

  $ . ./helpers.sh

  $ mkdir foo
  $ echo "y" > foo/x

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (source (copy $PWD/foo))
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run cp x %{prefix}/bin/x)))
  > EOF

  $ build_pkg test

  $ show_pkg test
  
  /source
  /source/x
  /target
  /target/bin
  /target/bin/x
  /target/cookie

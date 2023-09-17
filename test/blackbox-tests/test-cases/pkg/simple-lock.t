Test that we run the build command

  $ . ./helpers.sh

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)))
  > EOF

  $ build_pkg test

  $ show_pkg test
  
  /target
  /target/bin
  /target/bin/foo
  /target/cookie

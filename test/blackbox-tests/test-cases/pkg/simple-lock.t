Test that we run the build command

  $ . ./helpers.sh

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
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

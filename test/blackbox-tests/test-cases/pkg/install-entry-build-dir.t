Use build paths in the install entries of a package

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| cat >test.install <<EOF
  >          "\| bin: [ "?_build/install/default/bin/foo" ]
  >          "\| EOF
  >  ))
  > EOF

  $ build_pkg test

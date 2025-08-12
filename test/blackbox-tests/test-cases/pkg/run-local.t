Test that local commands, such as `./configure`, are called from the sandbox, as
they could have been created or modified by previous build commands (such as
`patch`)

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (write-file configure "#!/bin/sh\necho Package configured\n")
  >   (run chmod +x configure)
  >   (run ./configure)))
  > EOF

  $ build_pkg test
  Package configured

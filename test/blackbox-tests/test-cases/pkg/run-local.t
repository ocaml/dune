Test that local commands, such as `./configure`, are called from the sandbox, as
they could have been created or modified by previous build commands (such as
`patch`)

  $ . ./helpers.sh

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (write-file configure "#!/bin/sh\necho Package configured\n")
  >   (run chmod +x configure)
  >   (run sh configure)
  >   (run ./configure)))
  > EOF

  $ build_pkg test
      Building test.0.0.1
  Package configured
  Error:
  execve(../../../../../../../_private/default/.pkg/test/source/configure): No such file or directory
  -> required by _build/_private/default/.pkg/test/target
  [1]

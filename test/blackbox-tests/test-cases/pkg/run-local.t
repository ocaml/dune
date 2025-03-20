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
  >   (run ./configure)))
  > EOF

Now build the package per se. The output is suppressed for now as `./configure`
fails with an OS-dependent error (when it cannot find the script in the sources)
so we can only observe the failure until this is fixed.

  $ build_pkg test 2> /dev/null
  [1]

Note that this can be worked around for now calling explicitly `sh` instead:

  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (write-file configure "#!/bin/sh\necho Package configured\n")
  >   (run chmod +x configure)
  >   (run sh configure)))
  > EOF
  $ build_pkg test
  Package configured

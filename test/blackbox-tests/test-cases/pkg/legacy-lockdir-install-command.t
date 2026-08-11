Legacy lock dirs written before the conditional lock file format store the
install command as a bare action. It must still run: the command is executed
and its output is visible.

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (run echo BUILDING))
  > (install
  >  (run echo INSTALLING))
  > EOF

  $ build_pkg test
  BUILDING
  INSTALLING

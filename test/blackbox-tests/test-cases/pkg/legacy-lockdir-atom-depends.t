Legacy lock dirs written before the conditional package-field format store
dependencies as bare atoms. They must still decode as unconditional.

  $ make_lockdir
  $ make_lockpkg a <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > EOF

  $ build_pkg a

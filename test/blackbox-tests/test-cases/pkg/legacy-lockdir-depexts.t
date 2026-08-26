Legacy lock dirs written before the conditional package-field format store
external dependencies as bare strings. They must decode as unconditional.

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (depexts unzip gnupg)
  > EOF

  $ dune show depexts
  gnupg
  unzip

Pulling projects should respect ignored directories.

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed


  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ mkdir -p _foo/subproject
  $ cat >_foo/dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > EOF

Although subproject has a dune-project, it lives in a data_only_dirs, so it
should be ignored.

  $ cat >_foo/dune <<EOF
  > (data_only_dirs subproject)
  > EOF

  $ cat >_foo/subproject/dune-project <<EOF
  > should not be parsed because it's ignored by the stanza above
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - foo.dev

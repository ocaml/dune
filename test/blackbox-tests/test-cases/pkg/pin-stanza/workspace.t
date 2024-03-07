It should be possible to include custom repos from the workspace:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _foo
  $ cat >_foo/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF

  $ cat >dune-workspace<<EOF
  > (lang dune 3.10)
  > (pin
  >  (name foo)
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > (lock_dir
  >  (pins foo)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ mkrepo
  $ add_mock_repo_if_needed

Note that sources in the projects are overriden by the workspace

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin ;; does not exist
  >  (url "file://$PWD/_does_not_exist")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - foo.dev

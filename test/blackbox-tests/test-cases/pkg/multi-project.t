Multiple projects support

  $ . ./helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir a
  $ cat >a/dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name a)
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name b)
  >  (depends a))
  > EOF

  $ dune pkg lock
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: b.dev
  - a -> (problem)
      No known implementations at all
  [1]

This should work without any toplevel projects as well:

  $ mkdir b
  $ mv dune-project b
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

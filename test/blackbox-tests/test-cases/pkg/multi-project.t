Multiple projects support

  $ . ./helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir a
  $ cat >a/dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name a))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name b)
  >  (depends a))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

This should work without any toplevel projects as well:

  $ mkdir b
  $ mv dune-project b
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

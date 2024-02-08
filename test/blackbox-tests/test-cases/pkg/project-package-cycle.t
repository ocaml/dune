Demonstrate how dune handles project dependency cycles in the same project

  $ . ./helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name a)
  >  (depends b))
  > (package
  >  (name b)
  >  (depends a))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

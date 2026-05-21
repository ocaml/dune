Upper bounds on dune are stripped before the solver evaluates them, so a
project depending on an older dune still locks against the running dune.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package
  >  (name foo)
  >  (depends
  >   (dune
  >    (< 3.0))))
  > EOF

  $ mkpkg dune 2.0.0

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)

We can pull multiple packages from a single source

  $ mkrepo
  $ add_mock_repo_if_needed


  $ mkdir _multiple
  $ cat >_multiple/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url file://$PWD/_multiple)
  >  (package (name foo))
  >  (package (name bar)))
  > (package
  >  (name main)
  >  (depends foo bar))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.dev
  - foo.dev

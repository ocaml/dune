A malformed repository version that cannot satisfy a local package constraint
must not prevent the solver from selecting a valid version.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo 1 <<'EOF'
  > EOF

Version 2 is malformed, but the local constraint makes it impossible before its
manifest needs to be loaded.

  $ mkpkg foo 2 <<'EOF'
  > depends: [
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends (foo (= 1))))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.1

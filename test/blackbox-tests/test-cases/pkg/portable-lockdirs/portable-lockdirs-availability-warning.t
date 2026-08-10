A malformed availability filter is reported once per package version even when
it is evaluated for several platforms and solver attempts.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo 1 <<'EOF'
  > available: "not-a-boolean"
  > EOF
  $ mkpkg foo 2
  $ write_portable_lockdirs_project

  $ dune pkg lock
  Warning: Ignoring package foo.1 as its "available" filter can't be resolved
  to a boolean value.
  available: "not-a-boolean"
  value_bool: "not-a-boolean"
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.2

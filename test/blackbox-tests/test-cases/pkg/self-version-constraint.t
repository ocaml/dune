Demonstrates constraints that self reference the version

  $ . ./helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo 1.0.0 <<EOF
  > depends: [ "bar" {= version} ]
  > EOF

  $ mkpkg bar 0.9.0
  $ mkpkg bar 1.0.0
  $ mkpkg bar 2.0.0

The version of foo that should be selected is 1.0.0

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF
  Solution for dune.lock:
  - bar.1.0.0
  - foo.1.0.0

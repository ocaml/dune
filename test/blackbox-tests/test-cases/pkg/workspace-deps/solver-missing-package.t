A mock-repository package depends on a package that exists neither in
the repository nor in the workspace. The solver should reject this.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg consumer <<EOF
  > depends: [ "nonexistent-pkg" ]
  > EOF

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name proj) (allow_empty) (depends consumer))
  > EOF

  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: nonexistent-pkg
  [1]


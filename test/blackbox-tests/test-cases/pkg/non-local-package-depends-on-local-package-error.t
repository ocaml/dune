Test that we produce an error message when a non-local package depends on a
local package.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg remote <<EOF
  > depends: [
  >  "local_b"
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name local_a)
  >  (depends remote))
  > (package
  >  (name local_b))
  > EOF

  $ dune pkg lock
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "remote" is not in the workspace but
  it depends on the package "local_b" which is in the workspace.
  [1]

[dune pkg lock] is asked to lock a project where a mock-repository
package depends on a workspace package.

  $ mkrepo
  $ add_mock_repo_if_needed

The mock package [mylock] declares the workspace package [util] as a
dependency:

  $ mkpkg mylock <<EOF
  > depends: [
  >  "util"
  > ]
  > EOF

The workspace has two packages: [main] depends on the mock package
[mylock], and [util] has no dependencies.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name main) (depends mylock))
  > (package (name util))
  > EOF

The solver rejects the dependency because [mylock] is outside the
workspace and [util] is inside it:

  $ dune pkg lock 2>&1
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "mylock" is not in the workspace but
  it depends on the package "util" which is in the workspace.
  [1]

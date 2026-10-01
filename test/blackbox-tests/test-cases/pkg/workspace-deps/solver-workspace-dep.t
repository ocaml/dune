[dune pkg lock] is asked to lock a project where a mock-repository
package depends on a workspace package.

  $ mkrepo
  $ add_mock_repo_if_needed

The graph crosses the workspace boundary twice:
main (workspace) -> mylock (locked) -> util (workspace) -> leaf (locked).
The repository also contains util, but solving must use the supplied workspace
version rather than lock another definition of that package.

  $ mkpkg leaf
  $ mkpkg util 99.0
  $ mkpkg mylock <<EOF
  > depends: [ "util" {= "1.2"} ]
  > EOF

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name main) (depends mylock))
  > (package (name util) (version 1.2) (depends leaf))
  > EOF

The solver rejects the dependency because [mylock] is outside the
workspace and [util] is inside it:

  $ dune pkg lock 2>&1
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "mylock" is not in the workspace but
  it depends on the package "util" which is in the workspace.
  [1]

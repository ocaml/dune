Test that we produce an error message when a non-local package depends on a
local package.

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

A hand-written portable lockdir must not bypass this check. A locked package
with the workspace package's name can be disabled on the current platform, so
the platform projection contains no name collision. Without an explicit check,
the combined traversal would interpret the locked dependency as the workspace
package and follow its non-test dependencies.

  $ mkpkg a
  $ mkpkg b
  $ solve_project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name root)
  >  (allow_empty)
  >  (depends a))
  > (package
  >  (name workspace_dep)
  >  (allow_empty)
  >  (depends (b (= :with-test false))))
  > EOF
  Solution for dune.lock:
  - a.0.0.1

Make a depend on the workspace package. Add b and a same-named locked package
which is disabled on the current platform so the lockdir remains globally
closed under dependencies.

  $ cat > dune.lock/a.0.0.1.pkg <<EOF
  > (version 0.0.1)
  > (depends (all_platforms (workspace_dep)))
  > EOF
  $ cat > dune.lock/b.0.0.1.pkg <<EOF
  > (version 0.0.1)
  > EOF
  $ cat > dune.lock/workspace_dep.0.0.1.pkg <<EOF
  > (version 0.0.1)
  > (enabled_on_platforms (only ((os never))))
  > EOF

Validation and dependency queries reject the unsupported dependency before the
combined traversal can reinterpret it and make b reachable from root.

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune.lock/a.0.0.1.pkg", line 2, characters 25-38:
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "a" is not in the workspace but it
  depends on the package "workspace_dep" which is in the workspace.
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

  $ dune describe pkg list-locked-dependencies --transitive
  File "dune.lock/a.0.0.1.pkg", line 2, characters 25-38:
  2 | (depends (all_platforms (workspace_dep)))
                               ^^^^^^^^^^^^^
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "a" is not in the workspace but it
  depends on the package "workspace_dep" which is in the workspace.
  [1]

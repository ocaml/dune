A lock-dir package depends on a workspace library package.

  $ make_workspace_lib_package

The lock dir contains one package "consumer" that declares
"workspace-lib" as a dependency:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build (run echo "building consumer"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

Lock-dir validation does not currently recognise workspace packages as
valid dependency targets:

  $ dune build out 2>&1
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 9-22:
  The package "consumer" depends on the package "workspace-lib", but
  "workspace-lib" does not appear in the lockdir
  _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

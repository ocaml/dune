When a package name appears in both the lockdir and the workspace,
dependency resolution would otherwise be ambiguous. Dune rejects the edge
rather than silently choosing the lockdir package or the workspace package.

A workspace package "shared" defines a library:

  $ make_dune_project_with_package 3.24 shared
  $ mkdir src
  $ cat > src/dune <<EOF
  > (library (name shared) (public_name shared))
  > EOF
  $ cat > src/shared.ml <<EOF
  > let from_workspace = "ws"
  > EOF

The lock dir contains a package also called "shared":

  $ make_lockdir
  $ make_lockpkg shared <<EOF
  > (version 0.0.1)
  > (build (run echo "building shared from lockdir"))
  > EOF

A consumer lock-dir package depends on "shared":

  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends shared)
  > (build (run echo "building consumer"))
  > EOF

  $ write_lockdir_consumer_rule

The dependency is rejected before the lockdir build of "shared" can shadow the
workspace package:

  $ dune build out
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 9-15:
  The package "consumer" depends on the package "shared", but "shared" appears
  both in the lockdir _build/_private/default/.lock/dune.lock and in the
  workspace.
  Error: A package dependency cannot be resolved unambiguously because the same
  package name exists in both the lockdir and the workspace.
  [1]

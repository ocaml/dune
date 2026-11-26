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

The workspace package supplies the missing dependency without a lock entry.

  $ dune build out
  building consumer

Inactive conditional workspace dependencies do not become materialisation roots.

  $ make_named_package_project workspace-lib 3.25 '(absent (= :os "never"))'
  $ dune build out

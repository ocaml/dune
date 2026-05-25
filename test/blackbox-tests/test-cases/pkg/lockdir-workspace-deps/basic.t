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

The build succeeds. The rule depends on consumer's cookie; the
workspace install layout is materialized as part of consumer's build:

  $ dune build out
  building consumer

  $ dune rules --format=json _build/default/out | jq 'include "dune"; .[] | ruleDepFilePaths' | censor | sort
  "_build/_private/default/.pkg/consumer.0.0.1-$DIGEST/target/cookie"

  $ find _build/install/default/.packages -name '*.cmi' -o -name 'META' | censor | sort
  _build/install/default/.packages/$DIGEST/lib/workspace-lib/META
  _build/install/default/.packages/$DIGEST/lib/workspace-lib/workspace_lib.cmi

Editing the workspace library and rebuilding incrementally still works:

  $ cat > src/workspace_lib.ml <<EOF
  > let greeting = "Updated!"
  > let version = 2
  > EOF

  $ dune build out
  building consumer

The full build including the lock-dir consumer also succeeds:

  $ dune build @install

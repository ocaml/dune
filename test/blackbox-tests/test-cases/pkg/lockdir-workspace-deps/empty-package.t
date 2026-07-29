A lock-dir package depends on an empty workspace package (one declared
with [(allow_empty)] and no source content).

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-empty) (allow_empty))
  > EOF

The lock dir contains one package "consumer" that declares "ws-empty"
as a dependency:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends ws-empty)
  > (build (run echo "building consumer"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The build succeeds even when the workspace package is empty:

  $ dune build out
  building consumer

  $ find _build/install/default/.packages -type f -o -type l | censor | sort
  _build/install/default/.packages/$DIGEST/lib/ws-empty/META
  _build/install/default/.packages/$DIGEST/lib/ws-empty/dune-package


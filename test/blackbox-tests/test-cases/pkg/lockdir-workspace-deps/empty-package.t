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

An empty package still supplies its metadata through the materialiser.

  $ dune build out
  building consumer
  $ cat _build/default/out
  ok

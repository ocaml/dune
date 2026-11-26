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
  > (build
  >  (progn
  >   (run grep -q "(name ws-empty)" %{pkg:ws-empty:lib}/dune-package)
  >   (run echo "building consumer")))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The recipe reads the empty package's installed dune-package file through the
materialiser, even though the package has no source content.

  $ dune build out
  building consumer

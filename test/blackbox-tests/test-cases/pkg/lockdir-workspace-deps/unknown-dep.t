A lock-dir package declares a dependency on a name that does not exist
anywhere: not in the lockdir, not in the workspace, not dune, not a
system-provided package. Lockdir validation rejects this at load time
with a located error rather than letting the build silently produce an
empty install layout.

  $ make_dune_project 3.24

The lock dir contains one package "consumer" that declares a dep on
"does-not-exist":

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends does-not-exist)
  > (build (run echo "building consumer"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

  $ dune build out
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 9-23:
  The package "consumer" depends on the package "does-not-exist", but
  "does-not-exist" does not appear in the lockdir
  _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

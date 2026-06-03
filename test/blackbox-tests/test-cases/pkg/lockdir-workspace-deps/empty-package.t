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

  $ cat > dune <<EOF
  > (rule
  >  (deps (package consumer))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF

Lock-dir validation does not currently recognise workspace packages as
valid dependency targets:

  $ dune build out 2>&1
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 9-17:
  The package "consumer" depends on the package "ws-empty", but "ws-empty" does
  not appear in the lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

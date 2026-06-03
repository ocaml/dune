A lock-dir package depends on a workspace package that provides a
public executable.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-tool))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (executable
  >  (name main)
  >  (package ws-tool)
  >  (public_name ws-tool))
  > EOF
  $ cat > src/main.ml <<EOF
  > let () = print_endline "Hello from ws-tool!"
  > EOF

The lock dir contains one package "consumer" that declares "ws-tool"
as a dependency:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends ws-tool)
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
  characters 9-16:
  The package "consumer" depends on the package "ws-tool", but "ws-tool" does
  not appear in the lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

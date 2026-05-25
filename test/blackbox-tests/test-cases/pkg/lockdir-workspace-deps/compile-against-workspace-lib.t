A lock-dir package depends on a workspace library and its build
action inspects [OCAMLPATH].

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name workspace-lib))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (library
  >  (name workspace_lib)
  >  (public_name workspace-lib))
  > EOF
  $ cat > src/workspace_lib.ml <<EOF
  > let greeting = "Hello from workspace-lib!"
  > EOF

The lock dir contains one package "consumer" whose build action
writes [OCAMLPATH] to a file and installs it. The intent is that the
file should later be inspectable to confirm the workspace install
layout was prepended.

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (system "echo \$OCAMLPATH > ocamlpath.txt"))
  > (install
  >  (system "mkdir -p %{lib}/consumer && cp ocamlpath.txt %{lib}/consumer/"))
  > EOF

A rule depends on the lock-dir package:

  $ cat > dune <<EOF
  > (rule
  >  (deps (package consumer))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF

Lock-dir validation does not currently recognise workspace packages as
valid dependency targets, so the consumer's build action never runs:

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

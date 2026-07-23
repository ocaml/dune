A lock-dir package depends on a workspace package.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name myws))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (library (public_name myws))
  > EOF
  $ cat > src/myws.ml <<EOF
  > let x = 1
  > EOF

The lock dir contains one package "mylock" that declares "myws" as a
dependency:

  $ make_lockdir
  $ make_lockpkg mylock <<EOF
  > (version 0.0.1)
  > (depends myws)
  > (build (run echo "building mylock"))
  > EOF

A rule depends on the lock-dir package:

  $ cat > dune <<EOF
  > (rule
  >  (alias check)
  >  (deps (package mylock))
  >  (action (with-stdout-to out (echo "done"))))
  > EOF

Building rejects the lock dir because "myws" is a dependency of "mylock" but is
not itself a package in the lock dir. Lock-dir validation does not currently
recognise workspace packages as valid dependency targets.

  $ dune build @check 2>&1
  File "_build/_private/default/.lock/dune.lock/mylock.pkg", line 2, characters
  9-13:
  The package "mylock" depends on the package "myws", but "myws" does not
  appear in the lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

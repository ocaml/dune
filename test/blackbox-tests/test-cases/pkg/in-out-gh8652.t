Demonstrate the in-out bug reported in ocaml/dune#8652 where a locked package
depends on a workspace package. This creates an "in-out" problem: workspace ->
lock dir -> workspace.

The scenario:
- workspace package `a` depends on locked package `b`
- locked package `b` depends on workspace package `c`

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (package
  >  (name a)
  >  (allow_empty)
  >  (depends b))
  > (package
  >  (name c)
  >  (allow_empty))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.22)
  > EOF
  $ enable_pkg
  $ make_lockdir

Package `b` is in the lock dir and depends on workspace package `c`.

  $ make_lockpkg b << EOF
  > (version 0.0.1)
  > (depends c)
  > EOF

Attempting to build `b` fails. The lockdir validation code sees that `c` is not
in the lockdir and rejects it, even though `c` is a workspace package.

  $ build_pkg b
  File "_build/_private/default/.lock/dune.lock/b.pkg", line 2, characters
  9-10:
  The package "b" depends on the package "c", but "c" does not appear in the
  lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

  $ dune build
  File "_build/_private/default/.lock/dune.lock/b.pkg", line 2, characters
  9-10:
  The package "b" depends on the package "c", but "c" does not appear in the
  lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

The same happens even when `b` is a real dune package with `(libraries c)` in
its dune file. The lockdir validation error is correctly seen here, even though in the absence of validation we'd fail with a "Library c not found" error.

  $ mkdir c
  $ cat > c/dune << EOF
  > (library (public_name c))
  > EOF
  $ cat > c/c.ml << EOF
  > let hello = "hello from c"
  > EOF

  $ mkdir b-src
  $ cat > b-src/dune-project << EOF
  > (lang dune 3.22)
  > (package (name b))
  > EOF
  $ cat > b-src/dune << EOF
  > (library (public_name b) (libraries c))
  > EOF
  $ cat > b-src/b.ml << EOF
  > let greeting = C.hello
  > EOF

  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (depends c)
  > (dune)
  > (source (copy $PWD/b-src))
  > EOF

  $ build_pkg b
  File "_build/_private/default/.lock/dune.lock/b.pkg", line 2, characters
  9-10:
  The package "b" depends on the package "c", but "c" does not appear in the
  lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

A lockdir dep that is neither in the lockdir nor a workspace package is still
rejected by validation, with a similar error as the workspace package.

  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (depends does-not-exist)
  > EOF

  $ build_pkg b
  File "_build/_private/default/.lock/dune.lock/b.pkg", line 2, characters
  9-23:
  The package "b" depends on the package "does-not-exist", but "does-not-exist"
  does not appear in the lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

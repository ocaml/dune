Check that if the user tampers with the lockdir in a way that invalidates the
package graph then it's caught when loading the lockdir.

  $ . ./helpers.sh


  $ make_lockdir

  $ cat >dune.lock/a.pkg <<EOF
  > (deps b)
  > EOF
  $ cat >dune.lock/b.pkg <<EOF
  > (deps c)
  > EOF
  $ cat >dune.lock/c.pkg <<EOF
  > (deps a)
  > EOF

  $ dune describe pkg lock
  Contents of dune.lock:
  - a.dev
  - b.dev
  - c.dev

  $ cat >dune.lock/c.pkg <<EOF
  > (deps a d)
  > EOF

  $ dune describe pkg lock
  File "dune.lock/c.pkg", line 1, characters 8-9:
  The package "c" depends on the package "d", but "d" does not appear in the
  lockdir dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

  $ rm dune.lock/a.pkg

  $ dune describe pkg lock
  File "dune.lock/c.pkg", line 1, characters 6-7:
  The package "c" depends on the package "a", but "a" does not appear in the
  lockdir dune.lock.
  File "dune.lock/c.pkg", line 1, characters 8-9:
  The package "c" depends on the package "d", but "d" does not appear in the
  lockdir dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

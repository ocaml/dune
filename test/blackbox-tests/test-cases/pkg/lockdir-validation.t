Check that if the user tampers with the lockdir in a way that invalidates the
package graph then it's caught when loading the lockdir.

  $ . ./helpers.sh


  $ make_lockdir

  $ cat >dune.lock/a.pkg <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ cat >dune.lock/b.pkg <<EOF
  > (version 0.0.1)
  > (depends c)
  > EOF
  $ cat >dune.lock/c.pkg <<EOF
  > (version 0.0.1)
  > (depends a)
  > EOF

  $ dune describe pkg lock
  Contents of dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1

  $ cat >dune.lock/c.pkg <<EOF
  > (version 0.0.1)
  > (depends a d)
  > EOF

  $ dune describe pkg lock
  File "dune.lock/c.pkg", line 2, characters 11-12:
  The package "c" depends on the package "d", but "d" does not appear in the
  lockdir dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

  $ rm dune.lock/a.pkg

  $ dune describe pkg lock
  File "dune.lock/c.pkg", line 2, characters 9-10:
  The package "c" depends on the package "a", but "a" does not appear in the
  lockdir dune.lock.
  File "dune.lock/c.pkg", line 2, characters 11-12:
  The package "c" depends on the package "d", but "d" does not appear in the
  lockdir dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

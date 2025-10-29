Check that if the user tampers with the lockdir in a way that invalidates the
package graph then it's caught when loading the lockdir.

  $ . ./helpers.sh


  $ make_lockdir
  $ make_lockpkg a <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (depends c)
  > EOF
  $ make_lockpkg c <<EOF
  > (version 0.0.1)
  > (depends a)
  > EOF

  $ dune describe pkg lock
  Contents of .dune-solution-cache:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1

  $ make_lockpkg c <<EOF
  > (version 0.0.1)
  > (depends a d)
  > EOF

  $ dune describe pkg lock
  File ".dune-solution-cache/c.pkg", line 2, characters 11-12:
  The package "c" depends on the package "d", but "d" does not appear in the
  lockdir .dune-solution-cache.
  Error: At least one package dependency is itself not present as a package in
  the lockdir .dune-solution-cache.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

  $ rm ${source_lock_dir}/a.pkg

  $ dune describe pkg lock
  File ".dune-solution-cache/c.pkg", line 2, characters 9-10:
  The package "c" depends on the package "a", but "a" does not appear in the
  lockdir .dune-solution-cache.
  File ".dune-solution-cache/c.pkg", line 2, characters 11-12:
  The package "c" depends on the package "d", but "d" does not appear in the
  lockdir .dune-solution-cache.
  Error: At least one package dependency is itself not present as a package in
  the lockdir .dune-solution-cache.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

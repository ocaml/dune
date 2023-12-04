Create a lock directory that didn't originally exist
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  (no dependencies to lock)
  $ cat dune.lock/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))

Re-create a lock directory in the newly created lock dir
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  (no dependencies to lock)
  $ cat dune.lock/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))

Attempt to create a lock directory inside an existing directory without a lock.dune file
  $ rm -rf dune.lock
  $ cp -r dir-without-metadata dune.lock
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Error: Refusing to regenerate lock directory dune.lock
  Specified lock dir lacks metadata file (lock.dune)
  [1]

Attempt to create a lock directory inside an existing directory with an invalid lock.dune file
  $ rm -rf dune.lock
  $ cp -r dir-with-invalid-metadata dune.lock
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Error: Refusing to regenerate lock directory dune.lock
  Unable to parse lock directory metadata file (dune.lock/lock.dune):
  File "dune.lock/lock.dune", line 1, characters 0-12:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  [1]

Attempt to create a lock directory with the same name as an existing regular file
  $ rm -rf dune.lock
  $ touch dune.lock
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Error: Refusing to regenerate lock directory dune.lock
  Specified lock dir path is not a directory
  [1]

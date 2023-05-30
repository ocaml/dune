Create a lock directory that didn't originally exist
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --lock-dir=new-dir
  No dependencies to lock
  $ cat new-dir/lock.dune
  (lang package 0.1)

Re-create a lock directory in the newly created lock dir
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --lock-dir=new-dir
  No dependencies to lock
  $ cat new-dir/lock.dune
  (lang package 0.1)

Attempt to create a lock directory inside a directory without a lock.dune file
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --lock-dir=dir-without-metadata
  No dependencies to lock
  Error: Refusing to regenerate lock directory dir-without-metadata
  Specified lock dir lacks metadata file (lock.dune)
  [1]

Attempt to create a lock directory inside a directory with an invalid lock.dune file
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --lock-dir=dir-with-invalid-metadata
  No dependencies to lock
  Error: Refusing to regenerate lock directory dir-with-invalid-metadata
  File "dir-with-invalid-metadata/lock.dune", line 1, characters 0-12:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  [1]

Attempt to create a lock directory with the same name as a regular file
  $ touch regular-file
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --lock-dir=regular-file
  No dependencies to lock
  Error: Refusing to regenerate lock directory regular-file
  Specified lock dir path is not a directory
  [1]

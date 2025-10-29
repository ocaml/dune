Create a lock directory that didn't originally exist

  $ . ../helpers.sh

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path "dev/.dune-solution-cache")
  >  (repositories mock))
  > EOF
  $ add_mock_repo_if_needed

  $ dune pkg lock "dev/.dune-solution-cache"
  Solution for dev/.dune-solution-cache:
  (no dependencies to lock)
  $ dune pkg lock
  Solution for .dune-solution-cache:
  (no dependencies to lock)
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))

Re-create a lock directory in the newly created lock dir
  $ dune pkg lock
  Solution for .dune-solution-cache:
  (no dependencies to lock)
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))

Attempt to create a lock directory inside an existing directory without a lock.dune file

  $ rm -rf ${default_lock_dir}
  $ cp -r dir-without-metadata ${default_lock_dir}
  $ dune pkg lock
  Error: Refusing to regenerate lock directory .dune-solution-cache
  Specified lock dir lacks metadata file (lock.dune)
  [1]

Attempt to create a lock directory inside an existing directory with an invalid lock.dune file

  $ rm -rf ${default_lock_dir}
  $ cp -r dir-with-invalid-metadata ${default_lock_dir}
  $ dune pkg lock
  Error: Refusing to regenerate lock directory .dune-solution-cache
  Unable to parse lock directory metadata file (.dune-solution-cache/lock.dune):
  File ".dune-solution-cache/lock.dune", line 1, characters 0-12:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  [1]

Attempt to create a lock directory with the same name as an existing regular file

  $ rm -rf ${default_lock_dir}
  $ touch ${default_lock_dir}
  $ dune pkg lock
  Error: Refusing to regenerate lock directory .dune-solution-cache
  Specified lock dir path (.dune-solution-cache) is not a directory
  [1]

Create a lock directory that didn't originally exist

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path "dev/dune.lock")
  >  (repositories mock))
  > EOF
  $ add_mock_repo_if_needed

  $ dune_pkg_lock_normalized "dev/dune.lock"
  Solution for dev/dune.lock:
  (no dependencies to lock)
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))

Re-create a lock directory in the newly created lock dir
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))

Attempt to create a lock directory inside an existing directory without a lock.dune file

  $ rm -rf ${default_lock_dir}
  $ cp -r dir-without-metadata ${default_lock_dir}
  $ dune_pkg_lock_normalized
  Error: Refusing to regenerate lock directory dune.lock
  Specified lock dir lacks metadata file (lock.dune)
  [1]

Attempt to create a lock directory inside an existing directory with an invalid lock.dune file

  $ rm -rf ${default_lock_dir}
  $ cp -r dir-with-invalid-metadata ${default_lock_dir}
  $ dune_pkg_lock_normalized
  Error: Refusing to regenerate lock directory dune.lock
  Unable to parse lock directory metadata file (dune.lock/lock.dune):
  File "dune.lock/lock.dune", line 1, characters 0-12:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  [1]

Attempt to create a lock directory with the same name as an existing regular file

  $ rm -rf ${default_lock_dir}
  $ touch ${default_lock_dir}
  $ dune_pkg_lock_normalized
  Error: Refusing to regenerate lock directory dune.lock
  Specified lock dir path (dune.lock) is not a directory
  [1]

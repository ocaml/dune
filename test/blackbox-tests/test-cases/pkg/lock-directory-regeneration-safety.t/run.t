Create a lock directory that didn't originally exist

  $ cat >dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path "dev/dune.lock")
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF
  $ dune pkg lock "dev/dune.lock"
  Solution for dev/dune.lock:
  (no dependencies to lock)
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[Temp.create_temp_dir] called in a nonexistent directory",
     { dir = In_source_tree "dev/dune_f9d74c_lock" })
  Raised at Stdune__Result.ok_exn in file "otherlibs/stdune/src/result.ml",
    line 26, characters 15-22
  Called from Dune_pkg__Lock_dir.Write_disk.prepare.build in file
    "src/dune_pkg/lock_dir.ml", line 584, characters 26-53
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from Fiber__Core.O.(>>|).(fun) in file "vendor/fiber/src/core.ml",
    line 253, characters 36-41
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)
  $ cat dune.lock/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))

Re-create a lock directory in the newly created lock dir
  $ dune pkg lock
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
  $ dune pkg lock
  Error: Refusing to regenerate lock directory dune.lock
  Specified lock dir lacks metadata file (lock.dune)
  [1]

Attempt to create a lock directory inside an existing directory with an invalid lock.dune file
  $ rm -rf dune.lock
  $ cp -r dir-with-invalid-metadata dune.lock
  $ dune pkg lock
  Error: Refusing to regenerate lock directory dune.lock
  Unable to parse lock directory metadata file (dune.lock/lock.dune):
  File "dune.lock/lock.dune", line 1, characters 0-12:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  [1]

Attempt to create a lock directory with the same name as an existing regular file
  $ rm -rf dune.lock
  $ touch dune.lock
  $ dune pkg lock
  Error: Refusing to regenerate lock directory dune.lock
  Specified lock dir path (dune.lock) is not a directory
  [1]

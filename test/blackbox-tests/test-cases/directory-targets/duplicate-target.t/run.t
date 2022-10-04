Duplicate directory targets

  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.add_exn: key already exists", { key = "default/foo" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Stdlib__Map.Make.update in file "map.ml", line 283, characters
    18-28
  Called from Stdlib__Set.Make.fold in file "set.ml", line 383, characters
    34-55
  Called from Stdlib__Map.Make.fold in file "map.ml", line 321, characters
    19-42
  Called from Stdlib__Map.Make.fold in file "map.ml", line 321, characters
    19-42
  Called from Dune_engine__Load_rules.Load_rules.load_build_directory_exn.(fun)
    in file "src/dune_engine/load_rules.ml", line 636, characters 37-75
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
  -> required by ("load-dir", In_build_dir "default")
  -> required by ("build-alias", { dir = "default"; name = "default" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

Reproducing bug in https://github.com/ocaml/dune/issues/7600

When using (include_subdirs qualified), valid module names should be checked for
directories too.


  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF

  $ mkdir src

  $ cat > src/dune << EOF
  > (include_subdirs qualified)
  > (executable (name foo))
  > EOF

  $ cat > src/foo.ml

  $ mkdir src/baz-bar

  $ dune build @install
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Invalid Module_name.t", { s = "baz-bar" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 11, characters 30-62
  Called from Stdlib__List.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "otherlibs/stdune/src/list.ml", line 5,
    characters 19-33
  Called from Dune_rules__Ml_sources.make.(fun) in file
    "src/dune_rules/ml_sources.ml", line 459, characters 23-62
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_rules__Ml_sources.make in file
    "src/dune_rules/ml_sources.ml", line 457, characters 8-1023
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default/src")
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

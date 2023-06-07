The coq.extraction stanza should error if it overlaps with the coq.theory stanza

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > (using coq 0.8)
  > EOF

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo))
  > 
  > (coq.extraction
  >  (prelude foo)
  >  (extracted_modules))
  > EOF

  $ cat > foo.v

  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.add_exn: key already exists",
    { key =
        { source = In_build_dir "default/foo.v"; prefix = []; name = "foo" }
    })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 11, characters 30-62
  Called from Stdlib__Map.Make.update in file "map.ml", line 283, characters
    18-28
  Called from Dune_rules__Coq_sources.of_dir.(fun) in file
    "src/dune_rules/coq/coq_sources.ml", line 98, characters 20-75
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_rules__Dir_contents.Load.get0_impl.(fun) in file
    "src/dune_rules/dir_contents.ml", line 344, characters 31-120
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
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default")
  -> required by
     ("build-alias", { dir = In_build_dir "default"; name = "default" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

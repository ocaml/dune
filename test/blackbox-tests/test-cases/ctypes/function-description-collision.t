We create a buildable with a ctypes stanza that declares a duplicate
(function_description). This should display a nice error message instead of a
crash.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using ctypes 0.1)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name e)
  >  (ctypes
  >   (external_library_name ext)
  >   (build_flags_resolver (vendored))
  >   (type_description
  >    (functor T)
  >    (instance T))
  >   (function_description
  >    (functor F)
  >    (instance FI))
  >   (function_description
  >    (functor F)
  >    (instance FI))
  >   (generated_entry_point Entry)))
  > EOF

  $ touch e.ml

  $ cat > t.ml << EOF
  > module Types (_:Ctypes.TYPE) = struct
  > end
  > EOF

  $ cat > f.ml << EOF
  > module Functions (_:Ctypes.FOREIGN) = struct
  > end
  > EOF

  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.add_exn: key already exists",
    { key = "ext__c_cout_generated_functions__F__FI" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Stdlib__Map.Make.update in file "map.ml", line 283, characters
    18-28
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_rules__Foreign_sources.eval_foreign_stubs in file
    "src/dune_rules/foreign_sources.ml", line 132, characters 8-684
  Called from Dune_rules__Foreign_sources.make.(fun) in file
    "src/dune_rules/foreign_sources.ml", line 186, characters 14-120
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters
    24-34
  Called from Dune_rules__Foreign_sources.make in file
    "src/dune_rules/foreign_sources.ml", line 167, characters 6-973
  Called from Dune_rules__Dir_contents.Load.get0_impl.(fun) in file
    "src/dune_rules/dir_contents.ml", line 334, characters 31-191
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/scheduler.ml",
    line 73, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default")
  -> required by ("build-alias", { dir = "default"; name = "default" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

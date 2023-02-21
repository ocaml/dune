Library stanza error with `(stdlib ..)` and `(wrapped false)`

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ touch stdlib.opam
  $ cat > dune <<EOF
  > (library
  >  (public_name stdlib)
  >  (wrapped false)
  >  (stdlib
  >   (internal_modules Camlinternal*)))
  > EOF

  $ cat > stdlib.ml <<EOF
  > module A =  struct
  >   let x = 1
  > end
  > EOF

  $ dune build --display=short
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Option.value_exn", {})
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 11, characters 30-62
  Called from Dune_rules__Modules.lib in file "src/dune_rules/modules.ml", line
    806, characters 27-60
  Called from Dune_rules__Ml_sources.make_lib_modules in file
    "src/dune_rules/ml_sources.ml", line 370, characters 4-117
  Called from Stdune__Monad.Make.map.(fun) in file
    "otherlibs/stdune/src/monad.ml", line 14, characters 44-49
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


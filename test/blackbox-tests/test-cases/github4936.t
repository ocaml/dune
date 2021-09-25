C stubs and the tests stanza

  $ touch e.ml stubs.c
  $ cat > dune << EOF
  > (test
  >  (name e)
  >  (modes exe)
  >  (foreign_stubs
  >   (language c)
  >   (names stubs)))
  > EOF
  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.find_exn: failed to find key", { key = "e"; keys = [] })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune-unstable/code_error.ml", line 11, characters 30-62
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 288,
    characters 36-41
  Called from Fiber.Execution_context.apply in file "src/fiber/fiber.ml", line
    182, characters 9-14
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune-unstable/exn.ml", line 36, characters 27-56
  Called from Fiber.Execution_context.run_jobs in file "src/fiber/fiber.ml",
    line 204, characters 8-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune-unstable/exn.ml", line 36, characters 27-56
  Called from Fiber.Execution_context.run_jobs in file "src/fiber/fiber.ml",
    line 204, characters 8-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune-unstable/exn.ml", line 36, characters 27-56
  Called from Fiber.Execution_context.run_jobs in file "src/fiber/fiber.ml",
    line 204, characters 8-13
  -> required by ("load-dir", In_build_dir "default")
  -> required by ("build-alias", { dir = "default"; name = "default" })
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

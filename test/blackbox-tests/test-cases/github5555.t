This test is about `binaries` in `env` stanzas in `dune-workspace` files.

  $ t () {
  >   cat > dune-workspace << EOF
  > (lang dune $1)
  > (env ($2 (binaries x.exe)))
  > EOF
  >   dune build
  > }

In the default context, this produces an error.

  $ t 3.1 _
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[expander_for_artifacts] in [default_env] is undefined", {})
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
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

For explicit profiles too:

  $ t 3.1 dev
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[expander_for_artifacts] in [default_env] is undefined", {})
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
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

When the profile is not selected, this is ignored:

  $ t 3.1 other

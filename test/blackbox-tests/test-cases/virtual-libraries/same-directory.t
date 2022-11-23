Test that includes vlib and implementations all in the same folder.

  $ echo "(lang dune 3.6)" > dune-project

  $ touch empty.mli
  $ cat >dune <<EOF
  > (library
  >  (name impl_one)
  >  (implements vlib))
  > (library
  >  (name vlib)
  >  (virtual_modules empty))
  > EOF
  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("internal dependency cycle", { frames = [ ("<unnamed>", ()) ] })
  Raised at Memo.Exec.exec_dep_node.(fun) in file "src/memo/memo.ml", line
    1329, characters 31-64
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
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 73,
    characters 8-11
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

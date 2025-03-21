Exercise dune solving projects with version constraints on dune that aren't
satisfied by the currently-running dune.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a project that depends on a version of dune that must be earlier than the
current version of dune:
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package
  >  (name foo)
  >  (depends
  >   (dune
  >    (< 3.0))))
  > EOF

Make a mock dune package with a version that satisfies the constraint in the
project:
  $ mkpkg dune 2.0.0

Solve the dependencies:
  $ dune pkg lock
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("can't find a valid solution for the dependencies", { name = "foo" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Opam_solver.reject_unreachable_packages.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 2024-2026, characters 14-56
  Called from Stdlib__Map.Make.merge in file "map.ml", line 406, characters
    44-63
  Called from Dune_pkg__Opam_solver.reject_unreachable_packages.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 1993-2035, characters 6-32
  Called from Dune_pkg__Opam_solver.solve_lock_dir.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 2164-2168, characters 12-27
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

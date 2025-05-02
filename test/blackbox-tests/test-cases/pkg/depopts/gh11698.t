Reproduce the bug in #11698

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg fmt
  $ mkpkg semver

  $ mkdir _dep
  $ cat >_dep/dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name dep)
  >  (depopts fmt semver))
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.18)
  > (pin
  >  (url "file://$PWD/_dep")
  >  (package
  >   (name dep)
  >   (version 1.0.0)))
  > (package
  >  (name x)
  >  (depends dep))
  > EOF
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("invalid depopts",
     { depopts = And [ Atom ("fmt", Empty); Atom ("semver", Empty) ] })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Opam_solver.resolve_depopts.(fun) in file
    "src/dune_pkg/opam_solver.ml", line 1756, characters 4-16
  Called from Stdune__List.rev_concat_map.aux in file
    "otherlibs/stdune/src/list.ml", line 54, characters 15-18
  Called from Stdune__List.concat_map in file "otherlibs/stdune/src/list.ml",
    line 60, characters 26-47
  Called from Dune_pkg__Opam_solver.opam_package_to_lock_file_pkg in file
    "src/dune_pkg/opam_solver.ml", line 1837, characters 6-48
  Called from Stdlib__List.rev_map.rmap_f in file "list.ml" (inlined), line
    105, characters 22-25
  Called from Stdlib__List.rev_map in file "list.ml", line 107, characters 2-13
  Called from Stdune__List.map in file "otherlibs/stdune/src/list.ml", line 5,
    characters 19-33
  Called from Dune_pkg__Opam_solver.solve_lock_dir.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 2132-2139, characters 8-30
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 79, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 79, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

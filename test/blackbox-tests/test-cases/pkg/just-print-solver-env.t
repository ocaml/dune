Print the solver env when no dune-workspace is present
  $ dune pkg lock --just-print-solver-env
  Solver environment for context default:
  ((flags (with-doc with-test)) (sys ((opam-version 2.2.0~alpha-vendored))))

Add some build contexts with different environments
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name linux)
  >   (lock dune.linux.lock)
  >   (solver_env
  >    (sys
  >     (os linux)))))
  > (context
  >  (default
  >   (name no-doc)
  >   (lock dune.linux.lock)
  >   (solver_env
  >    (flags (:standard \ with-doc)))))
  > EOF

  $ dune pkg lock --all-contexts --just-print-solver-env
  Solver environment for context no-doc:
  ((flags (with-test)) (sys ((opam-version 2.2.0~alpha-vendored))))
  Solver environment for context linux:
  ((flags (with-doc with-test)) (sys ((opam-version 2.2.0~alpha-vendored) (os linux))))

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name attempt-to-override-opam-version)
  >   (lock dune.linux.lock)
  >   (solver_env
  >    (sys
  >     (opam-version foo)))))
  > EOF

  $ dune pkg lock --all-contexts --just-print-solver-env
  Error: Context attempt-to-override-opam-version would override solver
  variable opam-version. This variable may not be overriden.
  [1]

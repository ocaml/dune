Test that dune doesn't allow the "with-test" solver variable to be set by the
user.

Add a workspace that attempts to set the "with-test" variable:
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (path dune.lock)
  >  (solver_env
  >   (with-test false)))
  > (context
  >  (default
  >   (name default)
  >   (lock dune.lock)))
  > EOF

  $ dune pkg print-solver-env
  File "dune-workspace", line 5, characters 2-19:
  5 |   (with-test false)))
        ^^^^^^^^^^^^^^^^^
  Error: Setting the "with-test" solver variable is not permitted as it would
  conflict with dune's internal use of "with-test" while solving opam packages.
  [1]

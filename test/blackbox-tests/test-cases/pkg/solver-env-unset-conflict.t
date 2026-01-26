A variable cannot appear in both solver_env and unset_solver_vars.

  $ mkrepo

  $ mkpkg foo

  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.18)
  > (lock_dir
  >  (repositories mock)
  >  (solver_env
  >   (os linux))
  >  (unset_solver_vars os))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune pkg lock
  File "dune-workspace", line 6, characters 20-22:
  6 |  (unset_solver_vars os))
                          ^^
  Error: Variable "os" appears in both 'solver_env' and 'unset_solver_vars'
  which is not allowed.
  [1]

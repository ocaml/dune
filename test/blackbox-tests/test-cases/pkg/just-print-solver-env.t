Print the solver env when no dune-workspace is present
  $ dune pkg print-solver-env --dont-poll-system-solver-variables
  Solver environment for context default:
  - opam-version = 2.2.0~alpha-vendored
  - with-doc = false

Add some build contexts with different environments
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (path dune.linux.lock)
  >  (solver_env
  >   (os linux)))
  > (lock_dir
  >  (path dune.linux.no-doc.lock)
  >  (solver_env
  >   (arch x86_64)
  >   (os linux)
  >   (os-family ubuntu)
  >   (os-distribution ubuntu)
  >   (os-version 22.04)))
  > (lock_dir
  >  (path change-opam-version.lock)
  >  (solver_env
  >   (opam-version 42)))
  > (context
  >  (default
  >   (name linux)
  >   (lock_dir dune.linux.lock)))
  > (context
  >  (default
  >   (name no-doc)
  >   (lock_dir dune.linux.no-doc.lock)))
  > (context
  >  (default
  >   (name change-opam-version)
  >   (lock_dir change-opam-version.lock)))
  > EOF

  $ dune pkg print-solver-env --all-contexts --dont-poll-system-solver-variables
  Solver environment for context change-opam-version:
  - opam-version = 42
  - with-doc = false
  Solver environment for context no-doc:
  - arch = x86_64
  - opam-version = 2.2.0~alpha-vendored
  - os = linux
  - os-distribution = ubuntu
  - os-family = ubuntu
  - os-version = 22.04
  - with-doc = false
  Solver environment for context linux:
  - opam-version = 2.2.0~alpha-vendored
  - os = linux
  - with-doc = false

Print the solver env when no dune-workspace is present
  $ dune pkg print-solver-env --dont-poll-system-solver-variables
  Solver environment for context default:
  - System Environment Variables
    - arch (unset)
    - os (unset)
    - os-version (unset)
    - os-distribution (unset)
    - os-family (unset)
  - Constants
    - opam-version = 2.2.0~alpha-vendored

Add some build contexts with different environments
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name linux)
  >   (lock dune.linux.lock)
  >   (solver_sys_vars
  >    (os linux))))
  > (context
  >  (default
  >   (name no-doc)
  >   (lock dune.linux.lock)
  >   (solver_sys_vars
  >    (arch x86_64)
  >    (os linux)
  >    (os-family ubuntu)
  >    (os-distribution ubuntu)
  >    (os-version 22.04))))
  > EOF

  $ dune pkg print-solver-env --all-contexts --dont-poll-system-solver-variables
  Solver environment for context no-doc:
  - System Environment Variables
    - arch = x86_64
    - os = linux
    - os-version = 22.04
    - os-distribution = ubuntu
    - os-family = ubuntu
  - Constants
    - opam-version = 2.2.0~alpha-vendored
  Solver environment for context linux:
  - System Environment Variables
    - arch (unset)
    - os = linux
    - os-version (unset)
    - os-distribution (unset)
    - os-family (unset)
  - Constants
    - opam-version = 2.2.0~alpha-vendored

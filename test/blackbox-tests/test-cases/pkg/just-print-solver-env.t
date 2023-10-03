Print the solver env when no dune-workspace is present
  $ dune pkg print-solver-env
  Solver environment for context default:
  - Flags
    - with-doc = true
    - with-test = true
  - System Environment Variables
    - arch (unset)
    - os (unset)
    - os-version (unset)
    - os-distribution (unset)
    - os-family (unset)
  - Constants
    - opam-version = 2.2.0~alpha-vendored
  - Repositories
       :standard

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

  $ dune pkg print-solver-env --all-contexts
  Solver environment for context no-doc:
  - Flags
    - with-doc = false
    - with-test = true
  - System Environment Variables
    - arch (unset)
    - os (unset)
    - os-version (unset)
    - os-distribution (unset)
    - os-family (unset)
  - Constants
    - opam-version = 2.2.0~alpha-vendored
  - Repositories
       :standard
  Solver environment for context linux:
  - Flags
    - with-doc = true
    - with-test = true
  - System Environment Variables
    - arch (unset)
    - os = linux
    - os-version (unset)
    - os-distribution (unset)
    - os-family (unset)
  - Constants
    - opam-version = 2.2.0~alpha-vendored
  - Repositories
       :standard

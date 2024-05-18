Add some build contexts with different environments
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (path dune.lock)
  >  (unset_solver_vars arch os-distribution os os-family os-version sys-ocaml-version))
  > (lock_dir
  >  (path dune.linux.lock)
  >  (solver_env
  >   (os linux))
  >  (unset_solver_vars arch os-distribution os-family os-version sys-ocaml-version))
  > (lock_dir
  >  (path dune.linux.no-doc.lock)
  >  (solver_env
  >   (arch x86_64)
  >   (os linux)
  >   (os-family ubuntu)
  >   (os-distribution ubuntu)
  >   (os-version 22.04)
  >   (sys-ocaml-version 5.0)))
  > (lock_dir
  >  (path change-opam-version.lock)
  >  (solver_env
  >   (opam-version 42))
  >  (unset_solver_vars arch os os-distribution os-family os-version sys-ocaml-version))
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

  $ dune pkg print-solver-env --all
  Solver environment for lock directory change-opam-version.lock:
  - opam-version = 42
  - with-dev-setup = false
  - with-doc = false
  Solver environment for lock directory dune.linux.lock:
  - opam-version = 2.2.0~alpha-vendored
  - os = linux
  - with-dev-setup = false
  - with-doc = false
  Solver environment for lock directory dune.linux.no-doc.lock:
  - arch = x86_64
  - opam-version = 2.2.0~alpha-vendored
  - os = linux
  - os-distribution = ubuntu
  - os-family = ubuntu
  - os-version = 22.04
  - sys-ocaml-version = 5.0
  - with-dev-setup = false
  - with-doc = false
  Solver environment for lock directory dune.lock:
  - opam-version = 2.2.0~alpha-vendored
  - with-dev-setup = false
  - with-doc = false

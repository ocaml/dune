Test that dune doesn't allow the "with-test" solver variable to be set by the
user.

Add a workspace that attempts to set the "with-test" variable:
  $ mkrepo
  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (with-test true)))
  > (context
  >  (default
  >   (name default)
  >   (lock_dir dune.lock)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg print-solver-env
  Solver environment for lock directory dune.lock:
  - arch = x86_64
  - opam-version = 2.2.0
  - os = linux
  - os-distribution = ubuntu
  - os-family = debian
  - os-version = 24.11
  - post = true
  - sys-ocaml-version = 5.4.0+fake
  - with-dev-setup = false
  - with-doc = false
  - with-test = true

  $ mkpkg foo
  $ mkpkg bar

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo-pack)
  >  (allow_empty)
  >  (depends
  >   foo
  >   (bar :with-test)))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - bar.0.0.1
  - foo.0.0.1

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (with-test false)))
  > (context
  >  (default
  >   (name default)
  >   (lock_dir dune.lock)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF


  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo-pack)
  >  (allow_empty)
  >  (depends
  >   foo 
  >   (bar :with-test)))
  > EOF

  $ dune pkg print-solver-env
  Solver environment for lock directory dune.lock:
  - arch = x86_64
  - opam-version = 2.2.0
  - os = linux
  - os-distribution = ubuntu
  - os-family = debian
  - os-version = 24.11
  - post = true
  - sys-ocaml-version = 5.4.0+fake
  - with-dev-setup = false
  - with-doc = false
  - with-test = false

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

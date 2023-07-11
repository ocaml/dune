
Helper shell function that generates an opam file for a package:
  $ mkpkg() {
  >   name=$1
  >   mkdir -p mock-opam-repository/packages/$name/$name.0.0.1
  >   cat >mock-opam-repository/packages/$name/$name.0.0.1/opam
  > }

Helper shell function to generate a dune-project file and generate lockdir:
  $ solve_project() {
  >   cat >dune-project
  >   dune pkg lock --opam-repository-path=mock-opam-repository --all-contexts
  > }

Create a workspace file with some contexts with different combinations of with-test and with-doc flags
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name default-solver-env)
  >   (lock default-solver-env.lock)))
  > (context
  >  (default
  >   (name default-solver-flags)
  >   (lock default-solver-flags.lock)
  >   (solver_env)))
  > (context
  >  (default
  >   (name empty-solver-flags)
  >   (lock empty-solver-flags.lock)
  >   (solver_env
  >    (flags))))
  > (context
  >  (default
  >   (name with-test-only)
  >   (lock with-test-only.lock)
  >   (solver_env
  >    (flags with-test))))
  > (context
  >  (default
  >   (name with-doc-only)
  >   (lock with-doc-only.lock)
  >   (solver_env
  >    (flags with-doc))))
  > (context
  >  (default
  >   (name with-doc-and-with-test)
  >   (lock with-doc-and-with-test.lock)
  >   (solver_env
  >    (flags with-doc with-test))))
  > (context
  >  (default
  >   (name with-standard-flags)
  >   (lock with-standard-flags.lock)
  >   (solver_env
  >    (flags :standard))))
  > EOF

Generate a mock opam repository
  $ mkdir -p mock-opam-repository/packages
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg regular-package <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg test-package <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg doc-package <<EOF
  > opam-version: "2.0"
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends
  >   regular-package
  >   (test-package :with-test)
  >   (doc-package :with-doc)))
  > EOF
  Solution for with-standard-flags.lock:
  doc-package.0.0.1
  regular-package.0.0.1
  test-package.0.0.1
  
  Solution for with-doc-and-with-test.lock:
  doc-package.0.0.1
  regular-package.0.0.1
  test-package.0.0.1
  
  Solution for with-doc-only.lock:
  doc-package.0.0.1
  regular-package.0.0.1
  
  Solution for with-test-only.lock:
  regular-package.0.0.1
  test-package.0.0.1
  
  Solution for empty-solver-flags.lock:
  regular-package.0.0.1
  
  Solution for default-solver-flags.lock:
  doc-package.0.0.1
  regular-package.0.0.1
  test-package.0.0.1
  
  Solution for default-solver-env.lock:
  doc-package.0.0.1
  regular-package.0.0.1
  test-package.0.0.1
  
  Solution for dune.lock:
  doc-package.0.0.1
  regular-package.0.0.1
  test-package.0.0.1
  

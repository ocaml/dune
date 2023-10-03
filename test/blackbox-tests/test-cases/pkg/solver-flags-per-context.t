  $ . ./helpers.sh
  $ mkrepo

!! Do not delete this one for the one in helpers.sh as it passes --all-contexts !!
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

  $ mkpkg regular-package <<EOF
  > EOF

  $ mkpkg test-package <<EOF
  > EOF

  $ mkpkg doc-package <<EOF
  > EOF

  $ solve regular-package "(test-package :with-test)" "(doc-package :with-doc)"
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
  

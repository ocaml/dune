Test that we can identify the test-only locked dependencies of a package

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg a 0.0.1 <<EOF
  > EOF
  $ mkpkg a 0.0.2 <<EOF
  > EOF
  $ mkpkg b <<EOF
  > EOF
  $ mkpkg c <<EOF
  > EOF
  $ mkpkg d <<EOF
  > EOF
  $ mkpkg foo <<EOF
  > depends: [ "a" "b" ]
  > EOF
  $ mkpkg bar <<EOF
  > depends: [ "b" "c"]
  > EOF
  $ mkpkg baz <<EOF
  > depends: [ "bar" ]
  > EOF
  $ mkpkg qux <<EOF
  > depends: [ "a" "b" "c" "d" ]
  > EOF
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name local_1)
  >  (depends
  >   local_2))
  > (package
  >  (name local_2)
  >  (depends
  >   baz
  >   (qux :with-test)))
  > EOF
  Solution for dune.lock:
  - a.0.0.2
  - b.0.0.1
  - bar.0.0.1
  - baz.0.0.1
  - c.0.0.1
  - d.0.0.1
  - qux.0.0.1
List immediate dependencies in the lockdir. This should mirror the information in dune-project.
  $ dune describe pkg list-locked-dependencies
  Dependencies of local packages locked in dune.lock
  - Immediate dependencies of local package local_1.dev
    - local_2.dev
    
  - Immediate dependencies of local package local_2.dev
    - baz.0.0.1
    - qux.0.0.1 (test only)
List transitive dependencies. Note that only immediate test-only dependencies
and their dependencies are included (not test-only dependencies of test-only
dependencies).
  $ dune describe pkg list-locked-dependencies --transitive
  Dependencies of local packages locked in dune.lock
  - Transitive dependencies of local package local_1.dev
    - b.0.0.1
    - bar.0.0.1
    - baz.0.0.1
    - c.0.0.1
    - local_2.dev
    
  - Transitive dependencies of local package local_2.dev
    - b.0.0.1
    - bar.0.0.1
    - baz.0.0.1
    - c.0.0.1
    - a.0.0.2 (test only)
    - d.0.0.1 (test only)
    - qux.0.0.1 (test only)
Test that we can detect the case where a local package depends on some package
only when with-test is false. This will result in an error because the solver
is run with with-test=true so the dependency won't even be in the lockdir.
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name local_1)
  >  (depends
  >   (foo (= :with-test false))
  >   bar))
  > EOF
  Solution for dune.lock:
  - b.0.0.1
  - bar.0.0.1
  - c.0.0.1
  $ dune describe pkg list-locked-dependencies
  File "dune-project", lines 2-6, characters 0-71:
  2 | (package
  3 |  (name local_1)
  4 |  (depends
  5 |   (foo (= :with-test false))
  6 |   bar))
  The dependencies of local package "local_1" could not be satisfied from the
  lockdir when the solver variable 'with_test' is set to 'false':
  Package "foo" is missing
  [1]
Test that we can detect the case where a local package depends on some package
with version constraints that differ depending on :with-test. This will result
in an error because the solver is ruw with with-test=true which means an
incompatible version of the dependency will be in the lockdir.
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name local_1)
  >  (depends
  >   (a (or (= 0.0.1) (and :with-test (= 0.0.2))))
  >   bar))
  > EOF
  Solution for dune.lock:
  - a.0.0.2
  - b.0.0.1
  - bar.0.0.1
  - c.0.0.1
  $ dune describe pkg list-locked-dependencies
  File "dune-project", lines 2-6, characters 0-90:
  2 | (package
  3 |  (name local_1)
  4 |  (depends
  5 |   (a (or (= 0.0.1) (and :with-test (= 0.0.2))))
  6 |   bar))
  The dependencies of local package "local_1" could not be satisfied from the
  lockdir when the solver variable 'with_test' is set to 'false':
  Found version "0.0.2" of package "a" which doesn't satisfy the required
  version constraint "= 0.0.1"
  [1]

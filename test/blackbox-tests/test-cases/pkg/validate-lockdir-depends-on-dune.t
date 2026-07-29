Reproduce internal error with dune pkg validate-lockdir in #11188.

  $ mkrepo
  $ mkpkg a <<EOF
  > depends: [ "dune" ]
  > EOF
  $ mkpkg dune

  $ solve_project<<EOF
  > (lang dune 3.20)
  > (package
  >  (name vscode)
  >  (depends
  >   a))
  > EOF
  Solution for dune.lock:
  - a.0.0.1

Dune is able to verify this lock directory correctly:
  $ dune pkg validate-lockdir

A direct dependency on dune from a workspace package is also intentionally
absent from the solver's lockdir:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name direct)
  >  (allow_empty)
  >  (depends dune))
  > EOF
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)

The solver evaluates the dependency formula with the running version of dune
injected, but validation resolves the same formula without it and fails:

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", lines 2-5, characters 0-55:
  Error: The dependencies of local package "direct" could not be satisfied from
  the lockdir:
  Package "dune" is missing
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

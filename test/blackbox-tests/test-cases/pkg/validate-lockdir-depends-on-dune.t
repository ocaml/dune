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

Validation evaluates the dependency formula the same way the solver does, with
the running version of dune injected, so the empty lockdir is accepted:

  $ dune pkg validate-lockdir

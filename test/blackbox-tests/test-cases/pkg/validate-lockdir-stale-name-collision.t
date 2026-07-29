A lockdir that is both out of sync with the project and contains a package
whose name is also a local package. The name collision is reported even
though it may be a mere symptom of the staleness: relocking would remove
the package from the lockdir.

  $ mkrepo
  $ mkpkg bar
  $ solve_project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1

Turn the locked dependency into a local package without relocking:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends bar))
  > (package
  >  (name bar)
  >  (allow_empty))
  > EOF

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", lines 6-8, characters 0-36:
  Error: A package named "bar" is defined locally but is also present in the
  lockdir
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

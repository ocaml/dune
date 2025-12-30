Exercise dune solving projects with version constraints on dune that aren't
satisfied by the currently-running dune.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a project that depends on a version of dune that must be earlier than the
current version of dune:
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package
  >  (name foo)
  >  (depends
  >   (dune
  >    (< 3.0))))
  > EOF

Make a mock dune package with a version that satisfies the constraint in the
project:
  $ mkpkg dune 2.0.0

Solve the dependencies:
  $ dune_pkg_lock_normalized | dune_cmd subst '3.[0-9]+' '3.XX'
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  Couldn't solve the package dependency formula.
  Selected candidates: foo.dev
  - dune -> (problem)
      User requested = 3.XX
      foo dev requires < 3.XX
      Rejected candidates:
        dune.3.XX: Incompatible with restriction: < 3.XX

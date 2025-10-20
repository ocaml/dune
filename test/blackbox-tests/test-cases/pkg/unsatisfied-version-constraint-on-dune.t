Exercise dune solving projects with version constraints on dune that aren't
satisfied by the currently-running dune.

  $ . ./helpers.sh
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
  $ dune pkg lock 2>&1 | sed -E 's/"3.[0-9]+"/"3.XX"/'
  File "default/.lock/_unknown_", line 1, characters 0-0:
  Error: The current version of Dune does not satisfy the version constraints
  for Dune in this project's dependencies.
  Details:
  Found version "3.XX" of package "dune" which doesn't satisfy the required
  version constraint "< 3.0"

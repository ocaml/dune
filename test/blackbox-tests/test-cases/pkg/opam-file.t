Tests for reading dependencies out of opam files

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

Make sure we can read an disjunction from the opam file and solve a project
respecting the filters.

  $ mkpkg a
  $ mkpkg b

A package that depends on a disjunction should solve correctly with either side
of the disjunction to be picked for a solution:

  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" | "b"
  > ]
  > EOF
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - a.0.0.1

With the right filters, the other side of the disjunction should get picked:

  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" {> "1.0"} | "b" {< "1.0"}
  > ]
  > EOF
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - b.0.0.1

Unreachable packages should not be included. In the next test, "b" should not
be included in the lock directory because it is a post dep. Those are only
necessary during solving.

  $ cat > x.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" {> "1.0"} | "b" {< "1.0" post}
  > ]
  > EOF
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)

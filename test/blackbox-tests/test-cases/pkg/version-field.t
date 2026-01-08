Demonstrate the version_preference field in the lock_dir stanza.

  $ mkrepo

  $ mkpkg foo 1.0.0
  $ mkpkg foo 2.0.0
  $ mkpkg foo 3.0.0
  $ mkpkg bar 0.1.0
  $ mkpkg bar 0.5.0

Without version_preference, newest versions are selected:

  $ solve_project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo bar))
  > EOF
  Solution for dune.lock:
  - bar.0.5.0
  - foo.3.0.0

With (version_preference oldest), oldest versions are selected:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.18)
  > (lock_dir
  >  (version_preference oldest)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.1.0
  - foo.1.0.0

Constraints are still respected with oldest preference:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.18)
  > (lock_dir
  >  (version_preference oldest)
  >  (constraints (foo (>= 2.0.0)))
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.1.0
  - foo.2.0.0

Transitive dependencies also respect the preference:

  $ mkpkg baz 1.0.0 <<EOF
  > depends: [ "foo" ]
  > EOF
  $ mkpkg baz 2.0.0 <<EOF
  > depends: [ "foo" ]
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends baz))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.18)
  > (lock_dir
  >  (version_preference oldest)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - baz.1.0.0
  - foo.1.0.0

It's possible to include additional packages or constraints in workspace files:

  $ . ./helpers.sh

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (constraints doesnotexist foo (bar (= 1.0.0)))
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ mkrepo

  $ mkpkg foo 1.0.0
  $ mkpkg bar 1.0.0
  $ mkpkg bar 1.9.1

Notice that the constraints field doesn't introduce additional packages. The
"doesnotexist" package isn't being pulled.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends foo bar))
  > EOF
  Solution for dune.lock:
  - bar.1.0.0
  - foo.1.0.0

Constraint negation is supported since 3.18:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.18)
  > (lock_dir
  >  (constraints doesnotexist (foo (not (= 1.0.0))) (bar (not (= 1.0.0))))
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

There are no valid version of foo at the moment:

  $ solve_project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo bar))
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.1.9.1 x.dev
  - foo -> (problem)
      No usable implementations:
        foo.1.0.0: Package does not satisfy constraints of local package x
  [1]

If we add one:

  $ mkpkg foo 0.9.0

  $ solve_project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo bar))
  > EOF
  Solution for dune.lock:
  - bar.1.9.1
  - foo.0.9.0

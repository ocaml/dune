Demonstrates constraints that self reference the version

  $ . ./helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo 1.0.0 <<EOF
  > depends: [ "bar" {= version} ]
  > EOF

  $ mkpkg bar 0.9.0
  $ mkpkg bar 1.0.0
  $ mkpkg bar 2.0.0

The version of foo that should be selected is 1.0.0

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF
  Solution for .dune-solution-cache:
  - bar.1.0.0
  - foo.1.0.0

By default, local packages have version `dev` so the following version
constraint on `foo` will fail:

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends (foo (= :version))))
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory .dune-solution-cache:
  Couldn't solve the package dependency formula.
  Selected candidates: x.dev
  - foo -> (problem)
      No usable implementations:
        foo.1.0.0: Package does not satisfy constraints of local package x
  [1]

But specifying a version for the local package `x` yields a solution:

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (generate_opam_files true)
  > (version 1.0.0)
  > (package
  >  (name x)
  >  (depends (foo (= :version))))
  > EOF
  Solution for .dune-solution-cache:
  - bar.1.0.0
  - foo.1.0.0

Same result if the version of package `x` is specified by the opam file instead
of the dune-project:

  $ dune build x.opam
  $ grep '^version' x.opam
  version: "1.0.0"

  $ mkpkg dune 3.11
  $ solve_project <<EOF
  > (lang dune 3.11)
  > EOF
  Solution for .dune-solution-cache:
  - bar.1.0.0
  - foo.1.0.0

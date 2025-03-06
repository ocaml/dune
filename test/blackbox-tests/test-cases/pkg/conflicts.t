The solver should repsect the (conflicts) field of the (package) stanza.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 0.0.1
  $ mkpkg bar << EOF
  > depends: [ "foo" ]
  > EOF

The solver should say no solution rather than just ignoring the conflict.

  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (conflicts foo)
  >  (depends bar))
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Package does not satisfy constraints of local package x
  [1]

There could be more than one conflict and they can have version constraints:

  $ mkpkg foo2 0.0.1
  $ mkpkg bar2 << EOF
  > depends: [ "foo2" ]
  > EOF

  $ solve_project << EOF
  > (lang dune 3.11)
  > (generate_opam_files true)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (conflicts (foo (< 0.2)) (foo2 (< 0.2)))
  >  (depends bar bar2))
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 bar2.0.0.1 x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Package does not satisfy constraints of local package x
  - foo2 -> (problem)
      No usable implementations:
        foo2.0.0.1: Package does not satisfy constraints of local package x
  [1]

When conflicts are obtained from an opam file instead of a dune-project,
the behaviour should be the same:

  $ dune build x.opam
  $ sed -n '/conflicts/,/]/p' x.opam
  conflicts: [
    "foo" {< "0.2"}
    "foo2" {< "0.2"}
  ]

Even though the conflicts are listed by opam without a `|` to indicate a
disjunction, either package is problematic:

  $ mkpkg dune 3.11
  $ solve_project <<EOF
  > (lang dune 3.11)
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 bar2.0.0.1 x.dev
  - dune -> dune.3.11
      User requested = 3.18
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Package does not satisfy constraints of local package x
  - foo2 -> (problem)
      No usable implementations:
        foo2.0.0.1: Package does not satisfy constraints of local package x
  [1]

Adding a new version of `foo` only resolves one conflict:

  $ mkpkg foo 0.2
  $ solve_project <<EOF
  > (lang dune 3.11)
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 bar2.0.0.1 foo.0.2 x.dev
  - dune -> dune.3.11
      User requested = 3.18
  - foo2 -> (problem)
      No usable implementations:
        foo2.0.0.1: Package does not satisfy constraints of local package x
  [1]

Addition of `foo2` to solve the last remaining conflict:

  $ mkpkg foo2 0.2
  $ solve_project <<EOF
  > (lang dune 3.11)
  > EOF
  Solution for dune.lock:
  - bar.0.0.1
  - bar2.0.0.1
  - foo.0.2
  - foo2.0.2

Same but checking that the latest versions of `foo` and `foo2` are not selected
due to the version constraints conflicts:

  $ solve_project << EOF
  > (lang dune 3.11)
  > (generate_opam_files true)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (conflicts (foo (>= 0.2)) (foo2 (>= 0.2)))
  >  (depends bar bar2))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1
  - bar2.0.0.1
  - foo.0.0.1
  - foo2.0.0.1

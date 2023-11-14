Tests that dune can detect when the lockdir no longer contains a valid solution
to local packges

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg a <<EOF
  > depends: [ "c" "d" ]
  > EOF
  $ mkpkg b <<EOF
  > EOF
  $ mkpkg c <<EOF
  > depends: [ "e" ]
  > EOF
  $ mkpkg d <<EOF
  > EOF
  $ mkpkg e <<EOF
  > EOF
  $ mkpkg f 0.0.1 <<EOF
  > EOF
  $ mkpkg f 0.0.2 <<EOF
  > EOF

Define some local packages.
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo) (depends a b))
  > (package (name bar) (depends foo c))
  > EOF

Without a lockdir this command prints a hint but exits successfully.
  $ dune pkg validate-lockdir
  No lockdirs to validate.

Make the lockdir.
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1

Initially the lockdir will be valid.
  $ dune pkg validate-lockdir

Add a file to the lockdir to cause the parser to fail.
  $ echo foo > dune.lock/bar.pkg
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/bar.pkg", line 1, characters 0-3:
  Error: S-expression of the form (<name> <values>...) expected
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Remove the file but corrupt the lockdir metadata file.
  $ rm dune.lock/bar.pkg
  $ echo foo >> dune.lock/lock.dune
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/lock.dune", line 6, characters 0-3:
  Error: S-expression of the form (<name> <values>...) expected
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ rm -r dune.lock
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Add a dependency that's not present in the lockdir.
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo) (depends a b))
  > (package (name bar) (depends foo c f))
  > EOF

This results in an invalid lockdir due to the missing package.
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 3, characters 0-38:
  The dependencies of local package "bar" could not be satisfied from the
  lockdir:
  Package "f" is missing
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  - f.0.0.2
  $ dune pkg validate-lockdir

Change the version of a dependency to one which isn't in the lockdir.
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo) (depends a b))
  > (package (name bar) (depends foo c (f (< 0.0.2))))
  > EOF

Now the lockdir is invalid as it doesn't contain the right version of "f".
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 3, characters 0-50:
  The dependencies of local package "bar" could not be satisfied from the
  lockdir:
  Found version "0.0.2" of package "f" which doesn't satisfy the required
  version constraint "< 0.0.2"
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  - f.0.0.1
  $ dune pkg validate-lockdir

Add a new local package with the same name as a locked package.
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo) (depends a b))
  > (package (name bar) (depends foo c (f (< 0.0.2))))
  > (package (name b))
  > EOF

The lockdir is invalid as the package "b" is now defined both locally and in the lockdir.
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 4, characters 0-18:
  A package named "b" is defined locally but is also present in the lockdir
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  - a.0.0.1
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  - f.0.0.1
  $ dune pkg validate-lockdir

Remove a dependency from a local package so that the lockdir contains an unneeded package.
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo) (depends b))
  > (package (name bar) (depends foo c (f (< 0.0.2))))
  > (package (name b))
  > EOF

The lockdir is invalid as it contains unnecessary packages.
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  The lockdir contains packages which are not among the transitive dependencies
  of any local package:
  - a.0.0.1
  - d.0.0.1
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  - c.0.0.1
  - e.0.0.1
  - f.0.0.1
  $ dune pkg validate-lockdir

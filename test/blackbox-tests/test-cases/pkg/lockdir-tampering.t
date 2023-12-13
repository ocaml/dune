Tests that dune can detect when the lockdir has diverged from the local package
dependencies due to tampering with the lockdir. These are cases that won't be
caught by checking the dependency hash.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg a <<EOF
  > depends: [ "c" "d" ]
  > EOF
  $ mkpkg b 0.0.1 <<EOF
  > EOF
  $ mkpkg b 0.0.2 <<EOF
  > EOF
  $ mkpkg c <<EOF
  > depends: [ "e" ]
  > EOF
  $ mkpkg d <<EOF
  > EOF
  $ mkpkg e <<EOF
  > EOF

Define some local packages.
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo) (depends a (b (>= 0.0.2))))
  > (package (name bar) (depends foo c))
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Without a lockdir this command prints a hint but exits successfully.
  $ dune pkg validate-lockdir
  No lockdirs to validate.

Make the lockdir.
  $ dune pkg lock
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
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
  File "dune.lock/lock.dune", line 8, characters 0-3:
  Error: S-expression of the form (<name> <values>...) expected
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ rm -r dune.lock
  $ dune pkg lock
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Remove a package from the lockdir.
  $ rm dune.lock/a.pkg

This results in an invalid lockdir due to the missing package.
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 2, characters 0-47:
  The dependencies of local package "foo" could not be satisfied from the
  lockdir:
  Package "a" is missing
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

  $ cat dune.lock/b.pkg
  (version 0.0.2)
Change the version of a dependency by modifying its lockfile.
  $ cat >dune.lock/b.pkg <<EOF
  > (version 0.0.1)
  > EOF

Now the lockdir is invalid as it doesn't contain the right version of "b".
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 2, characters 0-47:
  The dependencies of local package "foo" could not be satisfied from the
  lockdir:
  Found version "0.0.1" of package "b" which doesn't satisfy the required
  version constraint ">= 0.0.2"
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Add a package to the lockdir with the same name as a local package.
  $ cat >dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > EOF

The lockdir is invalid as the package "b" is now defined both locally and in the lockdir.
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 2, characters 0-47:
  A package named "foo" is defined locally but is also present in the lockdir
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Add a package to the lockdir which isn't part of the local package dependency hierarchy.
  $ cat >dune.lock/f.pkg <<EOF
  > (version 0.0.1)
  > EOF

The lockdir is invalid as it contains unnecessary packages.
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  The lockdir contains packages which are not among the transitive dependencies
  of any local package:
  - f.0.0.1
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.
  $ dune pkg lock
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

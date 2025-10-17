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
  $ add_mock_repo_if_needed

Without a lockdir this command prints a hint but exits successfully.
  $ dune pkg validate-lockdir
  No lockdirs to validate.

Some helper functions

  $ promote() {
  >  cp -r "${default_lock_dir}" "${source_lock_dir}"
  >  chmod -R u+w "${source_lock_dir}"
  > }

  $ lock_in_source() {
  >   rm -rf "${source_lock_dir}"
  >   dune pkg lock
  >   promote
  > }

Make the lockdir.

  $ lock_in_source
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1

Initially the lockdir will be valid.

  $ dune pkg validate-lockdir

Add a file to the lockdir to cause the parser to fail.

  $ make_lockpkg bar <<EOF
  > foo
  > EOF
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/bar.pkg", line 1, characters 0-3:
  Error: S-expression of the form (<name> <values>...) expected
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Remove the file but corrupt the lockdir metadata file.

  $ rm ${source_lock_dir}/bar.pkg
  $ echo foo >> ${source_lock_dir}/lock.dune
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/lock.dune", line 8, characters 0-3:
  Error: S-expression of the form (<name> <values>...) expected
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.

  $ lock_in_source
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Remove a package from the lockdir.

  $ rm ${source_lock_dir}/a.pkg

This results in an invalid lockdir due to the missing package.

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 2, characters 0-47:
  Error: The dependencies of local package "foo" could not be satisfied from
  the lockdir:
  Package "a" is missing
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.

  $ lock_in_source
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

  $ cat ${default_lock_dir}/b.pkg
  (version 0.0.2)

Change the version of a dependency by modifying its lockfile.

  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > EOF

Now the lockdir is invalid as it doesn't contain the right version of "b".

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 2, characters 0-47:
  Error: The dependencies of local package "foo" could not be satisfied from
  the lockdir:
  Found version "0.0.1" of package "b" which doesn't satisfy the required
  version constraint ">= 0.0.2"
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.

  $ lock_in_source
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Add a package to the lockdir with the same name as a local package.

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > EOF

The lockdir is invalid as the package "b" is now defined both locally and in the lockdir.

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune-project", line 2, characters 0-47:
  Error: A package named "foo" is defined locally but is also present in the
  lockdir
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result.

  $ lock_in_source
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

Add a package to the lockdir which isn't part of the local package dependency hierarchy.

  $ make_lockpkg f <<EOF
  > (version 0.0.1)
  > EOF

The lockdir is invalid as it contains unnecessary packages.

  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  Error: The lockdir contains packages which are not among the transitive
  dependencies of any local package:
  - f.0.0.1
  Hint: The lockdir no longer contains a solution for the local packages in
  this project. Regenerate the lockdir by running: 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Regenerate the lockdir and validate the result:

  $ lock_in_source
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1
  $ dune pkg validate-lockdir

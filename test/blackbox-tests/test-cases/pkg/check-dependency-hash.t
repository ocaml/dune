Tests that changing the dependencies of a project cause lockdir validation to
fail due to the dependency hash not matching the hash stored in the lockdir.

Dummy opam repo so we can generate lockdirs

  $ mkrepo
  $ mkpkg a <<EOF
  > EOF

Start with a project with a single package with no dependencies:
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))
  $ dune pkg validate-lockdir

Add a dependency to the project:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (depends a))
  > EOF
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  Error: This project has at least one non-local dependency but the lockdir
  doesn't contain a dependency hash.
  An example of a non-local dependency of this project is: a
  Hint: Regenerate the lockdir by running 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Add a non-local dependency to the package:
  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (depends a))
  > EOF
  Solution for dune.lock:
  - a.0.0.1
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (dependency_hash 7ba1cacd46bb2609d7b9735909c3b8a5)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))
  $ dune pkg validate-lockdir

Add a second dependency to the project:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (depends a b))
  > EOF
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune.lock/lock.dune", line 3, characters 17-49:
  Error: Dependency hash in lockdir does not match the hash of non-local
  dependencies of this project. The lockdir expects the non-local dependencies
  to hash to:
  7ba1cacd46bb2609d7b9735909c3b8a5
  ...but the non-local dependencies of this project hash to:
  d18946fdd9833ae312d309f654f11c1b
  Hint: Regenerate the lockdir by running 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Remove all dependencies from the project:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo))
  > EOF
  $ dune pkg validate-lockdir
  Lockdir dune.lock does not contain a solution for local packages:
  File "dune.lock/lock.dune", line 3, characters 17-49:
  Error: This project has no non-local dependencies yet the lockfile contains a
  dependency hash: 7ba1cacd46bb2609d7b9735909c3b8a5
  Hint: Regenerate the lockdir by running 'dune pkg lock'
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Exercise handling invalid dependency hashes.
  $ make_lock_metadata_with_hash() {
  >   cat > ${source_lock_dir}/lock.dune <<EOF
  > (lang package 0.1)
  > (dependency_hash $1)
  > (repositories
  >  (complete false)
  >  (used))
  > EOF
  > }

Case where the label ("md5") is missing:
  $ make_lock_metadata_with_hash badhash
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/lock.dune", line 2, characters 17-24:
  Error: Dependency hash is not a valid md5 hash: badhash
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Case where the label is not "md5":
  $ make_lock_metadata_with_hash foo=badhash
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/lock.dune", line 2, characters 17-28:
  Error: Dependency hash is not a valid md5 hash: foo=badhash
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Case where the hash is not a valid md5 hash:
  $ make_lock_metadata_with_hash md5=badhash
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/lock.dune", line 2, characters 17-28:
  Error: Dependency hash is not a valid md5 hash: md5=badhash
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

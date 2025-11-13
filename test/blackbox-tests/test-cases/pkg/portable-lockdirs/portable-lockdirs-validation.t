Exercise the `dune pkg validate-lockdir` command on portable lockdirs.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Define some interdependent opam packages:
  $ mkpkg a 0.0.1 <<EOF
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
  > (lang dune 3.20)
  > (package (name foo) (depends a (b (>= 0.0.2))))
  > (package (name bar) (depends foo c))
  > EOF

Solve dependencies:
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - a.0.0.1
  - b.0.0.2
  - c.0.0.1
  - d.0.0.1
  - e.0.0.1

Validate the lockdir. This will succeed because dune generates valid lockdirs.
  $ dune pkg validate-lockdir

Remove a package from the lockdir.
  $ rm ${source_lock_dir}/a.0.0.1.pkg

Validate the lockdir. This time dune detects that a required lockfile is missing.
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

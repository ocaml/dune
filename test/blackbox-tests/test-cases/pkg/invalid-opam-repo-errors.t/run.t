Test the error cases for invalid opam repositories

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test))
  > EOF
  > cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/directory-that-does-not-exist"))
  > EOF

  $ lock() {
  > out="$(dune pkg lock 2>&1)"
  > local code="$?"
  > echo "$out" | sed 's/character.*:/characters X-X:/g' \
  >   | sed 's/source ".*"/source ../g' \
  >   | grep -v "\^"
  > return $code
  > }

  $ lock
  File "dune-workspace", line 6, characters X-X:
  6 |  (source ..))
  Error:
  $TESTCASE_ROOT/directory-that-does-not-exist
  does not exist
  [1]

  $ touch empty
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/empty"))
  > EOF
  $ lock
  File "dune-workspace", line 6, characters X-X:
  6 |  (source ..))
  Error:
  $TESTCASE_ROOT/empty
  is not a directory
  [1]

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/no-packages-dir"))
  > EOF
  $ lock
  File "dune-workspace", line 6, characters X-X:
  6 |  (source ..))
  Error:
  $TESTCASE_ROOT/no-packages-dir
  doesn't look like a path to an opam repository as it lacks a subdirectory
  named "packages"
  [1]

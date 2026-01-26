Test the error cases for invalid opam repositories

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test))
  > EOF
  $ add_mock_repo_if_needed "file://$(pwd)/directory-that-does-not-exist"

  $ lock() {
  > out="$(dune pkg lock 2>&1)"
  > local code="$?"
  > echo "$out" \
  >   | dune_cmd subst 'character.*:' 'characters X-X:' \
  >   | dune_cmd subst 'url ".*"' 'url ..' \
  >   | grep -v "\^"
  > return $code
  > }

  $ lock
  File "dune-workspace", line 6, characters X-X:
  6 |  (url ..))
  Error:
  $TESTCASE_ROOT/directory-that-does-not-exist
  does not exist
  [1]

  $ touch empty
  $ rm dune-workspace
  $ add_mock_repo_if_needed "file://$(pwd)/empty"
  $ lock
  File "dune-workspace", line 6, characters X-X:
  6 |  (url ..))
  Error:
  $TESTCASE_ROOT/empty
  is not a directory
  [1]

  $ rm dune-workspace
  $ add_mock_repo_if_needed "file://$(pwd)/no-packages-dir"
  $ lock
  File "dune-workspace", line 6, characters X-X:
  6 |  (url ..))
  Error:
  $TESTCASE_ROOT/no-packages-dir
  doesn't look like a path to an opam repository as it lacks a subdirectory
  named "packages"
  [1]

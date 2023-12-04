Test the error cases for invalid opam repositories

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test))
  > EOF

  $ dune pkg lock --opam-repository-path=directory-that-does-not-exist
  Error:
  $TESTCASE_ROOT/directory-that-does-not-exist
  does not exist
  [1]

  $ touch empty
  $ dune pkg lock --opam-repository-path=empty
  Error:
  $TESTCASE_ROOT/empty
  is not a directory
  [1]

  $ dune pkg lock --opam-repository-path=no-packages-dir
  Error:
  $TESTCASE_ROOT/no-packages-dir
  doesn't look like a path to an opam repository as it lacks a subdirectory
  named "packages"
  [1]

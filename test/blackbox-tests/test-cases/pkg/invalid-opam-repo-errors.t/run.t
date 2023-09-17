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

  $ dune pkg lock --opam-repository-path=no-repo-file
  Error: File
  $TESTCASE_ROOT/no-repo-file/repo
  does not exist or can't be read
  [1]

  $ dune pkg lock --opam-repository-path=bad-repo-file
  Error: At
  $TESTCASE_ROOT/bad-repo-file/repo:1:4-1:8::
  Parse error
  [1]

  $ dune pkg lock --opam-repository-path=no-repo-version
  Error: The file
  $TESTCASE_ROOT/no-repo-version/repo
  lacks an "opam-version" field.
  Hint: Add `opam-version: "2.0"` to the file.
  [1]

  $ dune pkg lock --opam-repository-path=bad-repo-version
  Error: The file
  $TESTCASE_ROOT/bad-repo-version/repo
  specifies an opam-version which is too low (1.0). The minimum opam-version is
  2.0.
  Hint: Change the opam-version field to `opam-version: "2.0"`.
  [1]

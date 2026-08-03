This test demonstrates the behaviour when a Unix error is encountered when copying the
files/ directory from a package directory inside an opam repository.

  $ mkrepo

Make a package with a patch
  $ make_with_patch_package
We remove the read permissions for dir/ making sure to add them back if we exit
the test.

  $ trap "chmod +r $opam_repo/files/dir" EXIT
  $ chmod -r $opam_repo/files/dir

The error message should have a location for the opam repository.

This does not currently seem to be the case.

  $ cat >dune<<EOF
  > (dirs * \ mock-opam-repository)
  > EOF

  $ solve with-patch
  File "$TESTCASE_ROOT/mock-opam-repository/packages/with-patch/with-patch.0.0.1/files", line 1, characters 0-0:
  Error: Unable to read file in opam repository:
  opendir($TESTCASE_ROOT/mock-opam-repository/packages/with-patch/with-patch.0.0.1/files/dir): Permission denied
  [1]


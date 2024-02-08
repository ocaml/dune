This test demonstrates the behaviour when a Unix error is encountered when copying the
files/ directory from a package directory inside an opam repository.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a patch
  $ mkpkg with-patch <<EOF
  > EOF

  $ fname1="foo.patch"
  $ fname2="dir/bar.patch"
  $ opam_repo="$mock_packages/with-patch/with-patch.0.0.1"
  $ mkdir -p $opam_repo/files/dir
  $ cat >$opam_repo/files/$fname1 <<EOF
  > foo
  > EOF
  $ cat >$opam_repo/files/$fname2 <<EOF
  > bar
  > EOF
We remove the read permissions for dir/

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
 
Make sure to set permissions back so the sandbox can be cleaned up.

  $ chmod +r $opam_repo/files/dir

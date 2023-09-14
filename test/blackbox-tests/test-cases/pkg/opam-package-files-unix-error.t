This test demonstrates the behaviour when a Unix error is encountered when copying the
files/ directory from a package directory inside an opam repostory.

  $ . ./helpers.sh

Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF


Make a package with a patch
  $ mkpkg with-patch <<EOF
  > opam-version: "2.0"
  > EOF

  $ fname1="foo.patch"
  $ fname2="dir/bar.patch"
  $ opam_repo="mock-opam-repository/packages/with-patch/with-patch.0.0.1"
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

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-patch))
  > EOF
  Error: Unable to read file in opam repository:
  opendir($TESTCASE_ROOT/mock-opam-repository/packages/with-patch/with-patch.0.0.1/files/dir): Permission denied
  [1]
 
Make sure to set permissions back so the sandbox can be cleaned up.

  $ chmod +r $opam_repo/files/dir

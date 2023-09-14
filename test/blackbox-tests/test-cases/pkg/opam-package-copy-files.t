This test checks that the files in the files/ directory inside a package in an opam
repository are copied correctly to the dune.lock file.


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

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-patch))
  > EOF
  Solution for dune.lock:
  with-patch.0.0.1
  

We expect that the files in the files directory of the opam repository get copied to the
lock file. 

  $ lock_dir="dune.lock/with-patch.files"
  $ [ -d $lock_dir ] && cat $lock_dir/$fname1
  [1]
  $ [ -d $lock_dir ] && cat $lock_dir/$fname2
  [1]

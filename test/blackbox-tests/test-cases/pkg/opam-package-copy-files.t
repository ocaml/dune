This test checks that the files in the files/ directory inside a package in an opam
repository are copied correctly to the dune.lock file.

  $ mkrepo

Make a package with a patch
  $ make_with_patch_package

  $ solve with-patch
  Solution for dune.lock:
  - with-patch.0.0.1

We expect that the files in the files directory of the opam repository get copied to the
lock file. 

  $ lock_dir="${default_lock_dir}/with-patch.0.0.1.files"
  $ [ -d $lock_dir ] && cat $lock_dir/$fname1
  foo
  $ [ -d $lock_dir ] && cat $lock_dir/$fname2
  bar

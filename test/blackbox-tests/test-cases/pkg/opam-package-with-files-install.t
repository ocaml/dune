This test demonstrates a package where the .install file being created by the
file copying step rather than the build step.

  $ make_lockdir
  $ mkdir -p ${default_lock_dir}/foo.files

  $ touch ${default_lock_dir}/foo.files/foo.install
  $ echo "(version 0.0.1)" > ${default_lock_dir}/foo.pkg

The foo.install file in files/ should have been copied over.
  $ build_pkg foo

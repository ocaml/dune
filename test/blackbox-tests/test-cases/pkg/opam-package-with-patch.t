This test checks that the patches field of an opam file is correctly translated into the
appropriate build step.

  $ mkrepo

Make a package with a patch
  $ mkpkg with-patch <<EOF
  > patches: ["foo.patch"]
  > build: ["cat" "foo.ml"]
  > EOF

  $ mkdir -p $mock_packages/with-patch/with-patch.0.0.1/files
  $ write_wrong_to_right_patch $mock_packages/with-patch/with-patch.0.0.1/files/foo.patch

  $ solve with-patch
  Solution for dune.lock:
  - with-patch.0.0.1
  $ append_to_lockpkg with-patch.0.0.1 <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the patch action. 

  $ cat ${default_lock_dir}/with-patch.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (progn (patch foo.patch) (run cat foo.ml))))))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source
  $ cat > source/foo.ml <<EOF
  > This is wrong
  > EOF

  $ build_pkg with-patch 
  This is right

We test how opam files with substs fields together with patches fields are translated into
the dune.lock file. Opam allows substitution to happen before the patches phase, so we
must do the same.

  $ mkrepo

Make a package with a substs and patches field field 
  $ mkpkg with-substs-and-patches <<EOF
  > substs: ["foo.patch"]
  > patches: ["foo.patch"]
  > build: [ "sh" "-c" "[ -e foo.ml ] && cat foo.ml" ]
  > EOF

  $ opam_repo=$mock_packages/with-substs-and-patches/with-substs-and-patches.0.0.1

  $ solve with-substs-and-patches
  Solution for dune.lock:
  - with-substs-and-patches.0.0.1
  $ append_to_lockpkg with-substs-and-patches.0.0.1 <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the substitute and patch actions.

  $ cat ${default_lock_dir}/with-substs-and-patches.0.0.1.pkg 
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (substitute foo.patch.in foo.patch)
       (patch foo.patch)
       (run sh -c "[ -e foo.ml ] && cat foo.ml"))))))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source

  $ write_wrong_to_right_patch source/foo.patch.in

  $ cat > source/foo.ml <<EOF
  > This is wrong
  > EOF

The file foo.ml should have been built:

  $ build_pkg with-substs-and-patches
  This is right

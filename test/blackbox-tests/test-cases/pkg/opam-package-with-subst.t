We test how opam files with substs fields are translated into the dune.lock file.
 
  $ . ./helpers.sh
  $ mkrepo

Make a package with a substs field 
  $ mkpkg with-substs <<EOF
  > opam-version: "2.0"
  > substs: ["foo.ml"]
  > build: [ "sh" "-c" "[ -e foo.ml ] && cat foo.ml" ]
  > EOF

  $ opam_repo=$opam_repo/with-substs/with-substs.0.0.1

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-substs)) 
  > EOF
  Solution for dune.lock:
  with-substs.0.0.1
  
  $ cat >>dune.lock/with-substs.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the substitute action. The generation step currently doesn't
add this in.

  $ cat dune.lock/with-substs.pkg 
  (version 0.0.1)
  
  (build
   (progn
    (substitute foo.ml.in foo.ml)
    (run sh -c "[ -e foo.ml ] && cat foo.ml")))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source
  $ cat > source/foo.ml.in <<EOF
  > I have been substituted.
  > EOF

The file foo.ml should have been built:

  $ build_pkg with-substs 
  I have been substituted.

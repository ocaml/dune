We test how opam files with substs fields are translated into the dune.lock file.
 
  $ . ./helpers.sh
  $ mkrepo

Make a package with a substs field 
  $ mkpkg with-substs <<EOF
  > substs: ["foo.ml"]
  > build: [ "sh" "-c" "[ -e foo.ml ] && cat foo.ml" ]
  > EOF

  $ solve with-substs
  Solution for dune.lock:
  - with-substs.0.0.1
  $ cat >>dune.lock/with-substs.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the substitute action.
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

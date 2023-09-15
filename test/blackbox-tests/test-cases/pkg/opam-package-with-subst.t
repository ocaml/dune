We test how opam files with substs fields are translated into the dune.lock file.
 
  $ . ./helpers.sh

Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

Make a package with a substs field 
  $ mkpkg with-substs <<EOF
  > opam-version: "2.0"
  > substs: ["foo.ml"]
  > build: [ "sh" "-c" "[ -e foo.ml ] && cat foo.ml" ]
  > EOF

  $ opam_repo=mock-opam-repository/packages/with-substs/with-substs.0.0.1

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
   (run sh -c "[ -e foo.ml ] && cat foo.ml"))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source
  $ cat > source/foo.ml.in <<EOF
  > I have been substituted.
  > EOF

The file foo.ml should have been built:

  $ build_pkg with-substs 
  Command exited with code 1.
  -> required by _build/_private/default/.pkg/with-substs/target
  [1]

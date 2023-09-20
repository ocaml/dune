The root opam var occuring in a substition is not supported. We make sure that this has a
good error message. This cannot happen during the solving stage, however we can catch it
during building.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a substs field 
  $ mkpkg with-substs-root <<EOF
  > substs: ["foo.ml"]
  > build: [ "echo" "foo.ml" ]
  > EOF

  $ opam_repo=$opam_repo/with-substs-root/with-substs-root.0.0.1

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-substs-root)) 
  > EOF
  Solution for dune.lock:
  with-substs-root.0.0.1
  
The lockfile should contain the substitute action. 

  $ cat dune.lock/with-substs-root.pkg 
  (version 0.0.1)
  
  (build
   (progn
    (substitute foo.ml.in foo.ml)
    (run echo foo.ml)))

We add the root opam variable to the .in file:
  $ mkdir source
  $ cat > source/foo.ml.in <<EOF
  > %{root}%
  > EOF

The file foo.ml should have been built:

  $ build_pkg with-substs-root 2>&1 | head -n6
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("unable to serialize exception",
    { exn =
        "OpamSystem.File_not_found(\"$TESTCASE_ROOT/_build/.sandbox/32b841facd505f4493dd4a22d339f953/_private/default/.pkg/with-substs-root/source/foo.ml.in\")"
    })

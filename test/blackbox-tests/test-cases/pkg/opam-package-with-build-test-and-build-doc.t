Older opam files contain the deprecated `build-test` and `build-doc` fields which has
since been deprecated to the `build` field with a filter.

In this test we demonstrate that we don't currently do anything special with those fields.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg with-build-test-doc <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "Building" ]
  > build-doc: [ "echo" "Building doc" ]
  > build-test: [ "echo" "Building test" ]
  > EOF

  $ mkdir -p $mock_packages/with-build-test-doc/with-build-test-doc.0.0.1

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-build-test-doc))
  > EOF
  Solution for dune.lock:
  with-build-test-doc.0.0.1
  
The lockfile should contain the `build-test` and `build-doc` fields inside the build
action.

This is currently not the case. 

  $ cat dune.lock/with-build-test-doc.pkg 
  (version 0.0.1)
  
  (build
   (run echo Building))

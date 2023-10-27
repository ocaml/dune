We test how a package using the `with-test` variable behaves.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg  

  $ mkpkg with-test <<EOF
  > depends: [ "test-dep" {with-test} ]
  > build: [ "echo" "Building with-test" {with-test} ]
  > EOF
  $ mkpkg test-dep <<EOF
  > build: [ "echo" "Building test-dep" ]
  > EOF

Solving shows that we don't include the test dependency. We need to also be passing the
experimental translation of opam filters arg for the solution to be correct.
  $ solve_translate_opam_filters with-test 
  Solution for dune.lock:
  - with-test.0.0.1

The build instructions have a conditional action as expected.
  $ cat dune.lock/with-test.pkg 
  (version 0.0.1)
  
  (build
   (run
    echo
    (when %{pkg-self:with-test} "Building with-test")))

Building currently fails due to no value being given to the with-test variable
  $ dune build 
  File "dune.lock/with-test.pkg", line 6, characters 8-29:
  6 |   (when %{pkg-self:with-test} "Building with-test")))
              ^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-test"
  [1]

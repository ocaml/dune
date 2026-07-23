Test for the `dune describe tests` command
==========================================

Setup with various test configurations
--------------------------------------

  $ make_dune_project_with_package 3.21 test-pkg

Simple test
-----------

  $ cat >dune <<EOF
  > (test
  >  (name simple_test))
  > EOF

  $ touch simple_test.ml

Test with all fields and in a subdirectory
--------------------

  $ mkdir -p subdir

  $ cat >subdir/dune <<EOF
  > (test
  >  (name complex_test)
  >  (package test-pkg)
  >  (deps test_data.txt)
  >  (locks /tmp/test.lock)
  >  (enabled_if true)
  >  (action (run echo "custom action")))
  > EOF

  $ touch subdir/complex_test.ml
  $ echo "test data" > subdir/test_data.txt

Disabled test
-------------

  $ mkdir -p disabled

  $ cat >disabled/dune <<EOF
  > (test
  >  (name disabled_test)
  >  (enabled_if false))
  > EOF

  $ touch disabled/disabled_test.ml

Test with dependencies
---------------------

  $ mkdir -p with_deps

  $ cat >with_deps/dune <<EOF
  > (test
  >  (name deps_test)
  >  (deps 
  >   (file ../subdir/test_data.txt)
  >   (file dune)))
  > EOF

  $ touch with_deps/deps_test.ml

Test with multiple tests in one stanza
---------------------------------------

  $ mkdir -p multi_tests

  $ cat >multi_tests/dune <<EOF
  > (tests
  >  (names test_a test_b test_c)
  >  (package test-pkg))
  > EOF

  $ touch multi_tests/test_a.ml
  $ touch multi_tests/test_b.ml
  $ touch multi_tests/test_c.ml

Testing unit tests (Tests.T)
=============================

Test sexp format for unit tests only:

  $ dune describe tests
  (((name deps_test)
    (source_dir with_deps)
    (package ())
    (enabled true)
    (location with_deps/dune:1)
    (target _build/default/with_deps/deps_test.exe))
   ((name complex_test)
    (source_dir subdir)
    (package (test-pkg))
    (enabled true)
    (location subdir/dune:1)
    (target _build/default/subdir/complex_test.exe))
   ((name test_a)
    (source_dir multi_tests)
    (package (test-pkg))
    (enabled true)
    (location multi_tests/dune:1)
    (target _build/default/multi_tests/test_a.exe))
   ((name test_b)
    (source_dir multi_tests)
    (package (test-pkg))
    (enabled true)
    (location multi_tests/dune:1)
    (target _build/default/multi_tests/test_b.exe))
   ((name test_c)
    (source_dir multi_tests)
    (package (test-pkg))
    (enabled true)
    (location multi_tests/dune:1)
    (target _build/default/multi_tests/test_c.exe))
   ((name disabled_test)
    (source_dir disabled)
    (package ())
    (enabled false)
    (location disabled/dune:1)
    (target _build/default/disabled/disabled_test.exe))
   ((name simple_test)
    (source_dir .)
    (package ())
    (enabled true)
    (location dune:1)
    (target _build/default/simple_test.exe)))

Test csexp format for unit tests:

  $ dune describe tests --format csexp | dune internal sexp-pp --format csexp
  (((name deps_test)
    (source_dir with_deps)
    (package ())
    (enabled true)
    (location with_deps/dune:1)
    (target _build/default/with_deps/deps_test.exe))
   ((name complex_test)
    (source_dir subdir)
    (package (test-pkg))
    (enabled true)
    (location subdir/dune:1)
    (target _build/default/subdir/complex_test.exe))
   ((name test_a)
    (source_dir multi_tests)
    (package (test-pkg))
    (enabled true)
    (location multi_tests/dune:1)
    (target _build/default/multi_tests/test_a.exe))
   ((name test_b)
    (source_dir multi_tests)
    (package (test-pkg))
    (enabled true)
    (location multi_tests/dune:1)
    (target _build/default/multi_tests/test_b.exe))
   ((name test_c)
    (source_dir multi_tests)
    (package (test-pkg))
    (enabled true)
    (location multi_tests/dune:1)
    (target _build/default/multi_tests/test_c.exe))
   ((name disabled_test)
    (source_dir disabled)
    (package ())
    (enabled false)
    (location disabled/dune:1)
    (target _build/default/disabled/disabled_test.exe))
   ((name simple_test)
    (source_dir .)
    (package ())
    (enabled true)
    (location dune:1)
    (target _build/default/simple_test.exe)))

Verify that disabled tests are still shown:

  $ dune describe tests | grep -A 5 disabled_test | head -6
   ((name disabled_test)
    (source_dir disabled)
    (package ())
    (enabled false)
    (location disabled/dune:1)
    (target _build/default/disabled/disabled_test.exe))

Clean up unit test files
-------------------------

Now remove all unit test files to prepare for CRAM tests:

  $ rm -rf with_deps subdir disabled multi_tests simple_test.ml dune

Testing CRAM tests (Cram_stanza.T)
===================================

Setup CRAM tests
----------------

Simple CRAM test:

  $ mkdir -p cram_tests

  $ cat >cram_tests/dune <<EOF
  > (cram
  >  (deps some_file.txt))
  > EOF

  $ touch cram_tests/some_file.txt

  $ cat >cram_tests/test.t <<EOF
  > Simple cram test
  >   \$ echo "hello"
  >   hello
  > EOF

CRAM test with package:

  $ mkdir -p cram_with_pkg

  $ cat >cram_with_pkg/dune <<EOF
  > (cram
  >  (package test-pkg))
  > EOF

  $ cat >cram_with_pkg/example.t <<EOF
  > Example test
  >   \$ echo "test"
  >   test
  > EOF

Disabled CRAM test:

  $ mkdir -p cram_disabled

  $ cat >cram_disabled/dune <<EOF
  > (cram
  >  (enabled_if false))
  > EOF

  $ cat >cram_disabled/disabled.t <<EOF
  > This should not run
  >   \$ echo "disabled"
  >   disabled
  > EOF

Test sexp format for CRAM tests:

  $ dune describe tests
  (((name cram)
    (source_dir cram_with_pkg)
    (package (test-pkg))
    (enabled true)
    (location cram_with_pkg/dune:1)
    (target @cram_with_pkg/runtest))
   ((name cram)
    (source_dir cram_tests)
    (package ())
    (enabled true)
    (location cram_tests/dune:1)
    (target @cram_tests/runtest))
   ((name cram)
    (source_dir cram_disabled)
    (package ())
    (enabled false)
    (location cram_disabled/dune:1)
    (target @cram_disabled/runtest)))

Test csexp format for CRAM tests:

  $ dune describe tests --format csexp | dune internal sexp-pp --format csexp
  (((name cram)
    (source_dir cram_with_pkg)
    (package (test-pkg))
    (enabled true)
    (location cram_with_pkg/dune:1)
    (target @cram_with_pkg/runtest))
   ((name cram)
    (source_dir cram_tests)
    (package ())
    (enabled true)
    (location cram_tests/dune:1)
    (target @cram_tests/runtest))
   ((name cram)
    (source_dir cram_disabled)
    (package ())
    (enabled false)
    (location cram_disabled/dune:1)
    (target @cram_disabled/runtest)))

Verify that disabled CRAM tests are still shown:

  $ dune describe tests | grep -A 5 cram_disabled | head -6
    (source_dir cram_disabled)
    (package ())
    (enabled false)
    (location cram_disabled/dune:1)
    (target @cram_disabled/runtest)))

Inline tests generated by ppx_expect

  $ mkdir -p inline_lib_test

  $ cat >inline_lib_test/dune <<EOF
  > (library
  >  (name mylib)
  >  (inline_tests)
  >  (preprocess (pps ppx_expect)))
  > EOF

  $ cat >inline_lib_test/mylib.ml <<EOF
  > let%expect_test _ =
  >   print_endline "Hello from ppx_expect!";
  >   [%expect {|
  >     Hello from ppx_expect!
  >   |}]
  > EOF

Test sexp format should include the inline-test runner alias for the library:

  $ dune describe tests | grep -A 5 mylib | head -6
  (((name mylib)
    (source_dir inline_lib_test)
    (package ())
    (enabled true)
    (location inline_lib_test/dune:1)
    (target @inline_lib_test/runtest-mylib))

  $ rm -rf inline_lib_test

Inline tests with ppx_inline_test
---------------------------------

  $ mkdir -p inline_ppx_test

  $ cat >inline_ppx_test/dune <<EOF
  > (library
  >  (name my_ppx_lib)
  >  (package test-pkg)
  >  (inline_tests)
  >  (preprocess (pps ppx_inline_test)))
  > EOF

  $ cat >inline_ppx_test/my_ppx_lib.ml <<EOF
  > let%test "my ppx test" = 1 + 1 = 2
  > EOF

Test sexp format for ppx_inline_test:

  $ dune describe tests | grep -A 5 my_ppx_lib | head -6
  (((name my_ppx_lib)
    (source_dir inline_ppx_test)
    (package (test-pkg))
    (enabled true)
    (location inline_ppx_test/dune:1)
    (target @inline_ppx_test/runtest-my_ppx_lib))

  $ rm -rf inline_ppx_test

Test error cases
----------------

Test invalid context:

  $ dune describe tests --context nonexistent
  Error: Context "nonexistent" not found!
  [1]

Test running tests by specifying ML source files directly.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Set up a simple test with multiple test executables:

  $ cat > dune <<EOF
  > (tests
  >  (names test1 test2)
  >  (modules test1 test2 test_lib))
  > EOF

  $ cat > test_lib.ml <<EOF
  > let run_test name =
  >   Printf.printf "Running %s\n" name
  > EOF

  $ cat > test1.ml <<EOF
  > let () =
  >   Test_lib.run_test "test1";
  >   assert true
  > EOF

  $ cat > test2.ml <<EOF
  > let () =
  >   Test_lib.run_test "test2";
  >   exit 1 (* This test fails *)
  > EOF

Running tests by specifying ML files directly:

  $ dune test test1.ml
  Running test1

  $ dune test test2.ml
  File "dune", line 2, characters 14-19:
  2 |  (names test1 test2)
                    ^^^^^
  Running test2
  [1]

Error when specifying a non-test ML file:

  $ cat > lib.ml <<EOF
  > let x = 42
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (modules lib))
  > (tests
  >  (names test1 test2)
  >  (modules test1 test2 test_lib))
  > EOF

  $ dune test lib.ml
  Error: "lib.ml" does not match any known test.
  [1]

Error when specifying a non-existent ML file:

  $ dune test nonexistent.ml
  Error: "nonexistent.ml" does not match any known test.
  [1]

  $ dune test test3.ml
  Error: "test3.ml" does not match any known test.
  Hint: did you mean test1.ml or test2.ml?
  [1]

Can run tests from _build directory:

  $ rm -rf _build
  $ dune test _build/default/test1.ml
  Running test1

Can specify multiple ML files:

  $ rm -rf _build
  $ dune test test1.ml test2.ml
  Running test1
  File "dune", line 5, characters 14-19:
  5 |  (names test1 test2)
                    ^^^^^
  Running test2
  [1]

--------------------------------------------------------------------------------

Test with a single test executable and multiple modules.

  $ cat > dune <<EOF
  > (test
  >  (name single_test)
  >  (modules single_test helper1 helper2))
  > EOF

  $ cat > helper1.ml <<EOF
  > let value = "from helper1"
  > EOF

  $ cat > helper2.ml <<EOF
  > let value = Helper1.value ^ " and helper2"
  > EOF

  $ cat > single_test.ml <<EOF
  > let () =
  >   Printf.printf "Test: %s\n" Helper2.value;
  >   exit 1
  > EOF

Targeting the entry point:

  $ dune test single_test.ml
  File "dune", line 2, characters 7-18:
  2 |  (name single_test)
             ^^^^^^^^^^^
  Test: from helper1 and helper2
  [1]

Targeting helper modules should also run the test:

  $ dune test helper1.ml
  File "dune", line 2, characters 7-18:
  2 |  (name single_test)
             ^^^^^^^^^^^
  Test: from helper1 and helper2
  [1]

  $ dune test helper2.ml
  File "dune", line 2, characters 7-18:
  2 |  (name single_test)
             ^^^^^^^^^^^
  Test: from helper1 and helper2
  [1]

--------------------------------------------------------------------------------

Test with more complex module dependencies.
Set up: A <- B <- C -> D -> E where A and E are entry points.

  $ cat > dune <<EOF
  > (tests
  >  (names a e)
  >  (modules a b c d e))
  > EOF

  $ cat > c.ml <<EOF
  > let shared = "shared by both"
  > EOF

  $ cat > b.ml <<EOF
  > let helper = "used only by A via " ^ C.shared
  > EOF

  $ cat > d.ml <<EOF
  > let helper = "used only by E via " ^ C.shared
  > EOF

  $ cat > a.ml <<EOF
  > let () =
  >   Printf.printf "A using %s\n" B.helper;
  >   exit 1
  > EOF

  $ cat > e.ml <<EOF
  > let () =
  >   Printf.printf "E using %s\n" D.helper;
  >   exit 1
  > EOF

Targeting entry points A and E should run those specific tests:

  $ dune test a.ml
  File "dune", line 2, characters 8-9:
  2 |  (names a e)
              ^
  A using used only by A via shared by both
  [1]

  $ dune test e.ml
  File "dune", line 2, characters 10-11:
  2 |  (names a e)
                ^
  E using used only by E via shared by both
  [1]

Targeting non-entry-point modules in a stanza with multiple tests errors.
Currently B, D, and C all error even though only C is actually shared:

  $ dune test b.ml
  Error: "b.ml" is used by multiple test executables and cannot be run
  directly.
  [1]

  $ dune test d.ml
  Error: "d.ml" is used by multiple test executables and cannot be run
  directly.
  [1]

  $ dune test c.ml
  Error: "c.ml" is used by multiple test executables and cannot be run
  directly.
  [1]


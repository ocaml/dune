Test running inline tests by specifying ML source files directly.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Set up a simple inline tests backend and libraries:

  $ cat > dune <<EOF
  > (library
  >  (name test_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (run sed "s/(\\\\*TEST:\\\\(.*\\\\)\\\\*)/let () = if \\"%{inline_tests}\\" = \\"enabled\\" then \\\\1;;/" %{impl-files}))))
  > 
  > (library
  >  (name mylib)
  >  (modules lib)
  >  (inline_tests (backend test_backend)))
  > 
  > (library
  >  (name regular_lib)
  >  (modules regular))
  > EOF

  $ cat > lib.ml <<EOF
  > let add x y = x + y
  > (*TEST: assert false *)
  > EOF

  $ cat > regular.ml <<EOF
  > let subtract x y = x - y
  > EOF

Error when specifying a library ML file without inline_tests:

  $ dune test regular.ml
  Error: "regular.ml" does not match any known test.
  [1]

When specifying a library with inline_tests, it should run the test (and fail):

  $ dune test lib.ml
  File "dune", line 10, characters 1-38:
  10 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

Error when specifying a non-existent ML file:

  $ dune test nonexistent.ml
  Error: "nonexistent.ml" does not match any known test.
  [1]

Test that inline tests are included in suggestions:

  $ dune test li.ml
  Error: "li.ml" does not match any known test.
  Hint: did you mean lib.ml?
  [1]

Can run inline tests from _build directory:

  $ dune test _build/default/lib.ml
  File "dune", line 10, characters 1-38:
  10 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

Test with multiple contexts:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (context (default))
  > (context (default (name alt)))
  > EOF

Running inline tests in multiple contexts:

  $ dune test lib.ml
  File "dune", line 10, characters 1-38:
  10 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  File "dune", line 10, characters 1-38:
  10 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

Running inline tests in a specific build directory:

  $ dune test _build/alt/lib.ml
  File "dune", line 10, characters 1-38:
  10 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

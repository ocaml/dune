Testing the interaction of dune runtest and inline tests.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat >test.ml <<EOF
  > (*TEST: assert (1 = 2) *)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name backend_simple)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (run sed "s/(\\\\*TEST:\\\\(.*\\\\)\\\\*)/let () = if \\"%{inline_tests}\\" = \\"enabled\\" then \\\\1;;/" %{impl-files}))))
  > 
  > (library
  >  (name foo_simple)
  >  (inline_tests (backend backend_simple)))
  > 
  > (env
  >  (ignore-inline-tests (inline_tests ignored))
  >  (enable-inline-tests (inline_tests enabled))
  >  (disable-inline-tests (inline_tests disabled)))
  > EOF

Dune runtest is able to run an inline test

  $ dune test foo_simple
  File "dune", line 9, characters 1-40:
  9 |  (inline_tests (backend backend_simple)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  [1]

Typos should suggest inline test:

  $ dune test foo_simple_
  Error: "foo_simple_" does not match any known test.
  Hint: did you mean foo_simple?
  [1]

Cannot run any library:

  $ dune test backend_simple
  Error: "backend_simple" does not match any known test.
  [1]

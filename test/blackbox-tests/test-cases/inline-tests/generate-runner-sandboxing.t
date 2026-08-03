The inline-tests runner generator becomes sandboxed starting with dune 3.23.

  $ cat > dune <<EOF
  > (library
  >  (name test_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner
  >    (run sed
  >     "s/(\\\\*TEST:\\\\(.*\\\\)\\\\*)/let () = if \\"%{inline_tests}\\" = \\"enabled\\" then \\\\1;;/"
  >     %{impl-files}))))
  > 
  > (library
  >  (name mylib)
  >  (modules lib)
  >  (inline_tests (backend test_backend)))
  > EOF

  $ cat > lib.ml <<EOF
  > let add x y = x + y
  > (*TEST: assert false *)
  > EOF

At dune 3.22 the generator still runs outside the sandbox.

  $ make_dune_project 3.22

  $ dune test lib.ml
  File "dune", line 13, characters 1-38:
  13 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select(.args.prog | basename | startswith("sed"))
  >   | (.args.dir | contains(".sandbox"))
  >   ][0]'
  false

At dune 3.23 the generator is sandboxed and %{impl-files} still works.

  $ rm -rf _build
  $ make_dune_project 3.23

  $ dune test lib.ml
  File "dune", line 13, characters 1-38:
  13 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select(.args.prog | basename | startswith("sed"))
  >   | (.args.dir | contains(".sandbox"))
  >   ][0]'
  true

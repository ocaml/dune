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

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ dune test lib.ml
  File "dune", line 13, characters 1-38:
  13 |  (inline_tests (backend test_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  [1]

  $ dune trace cat | jq -sc 'include "dune";
  >   [ .[]
  >   | processes
  >   | select(.args.prog | basename | startswith("sed"))
  >   | (.args.dir | contains(".sandbox"))
  >   ][0]'
  false

At dune 3.23 the generator is sandboxed, which exposes the missing input.

  $ rm -rf _build
  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ dune test lib.ml 2>&1 | sed -E 's#/.*/sed:#sed:#'
  File "dune", lines 10-13, characters 0-77:
  10 | (library
  11 |  (name mylib)
  12 |  (modules lib)
  13 |  (inline_tests (backend test_backend)))
  sed: can't read lib.ml: No such file or directory
  [1]

  $ dune trace cat | jq -sc 'include "dune";
  >   [ .[]
  >   | processes
  >   | select(.args.prog | basename | startswith("sed"))
  >   | (.args.dir | contains(".sandbox"))
  >   ][0]'
  true

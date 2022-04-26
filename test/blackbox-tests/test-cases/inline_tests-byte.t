Test running inline tests in bytecode mode

Reproduction case for #5515

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (modules test)
  >  (inline_tests (modes byte))
  >  (preprocess (pps ppx_inline_test)))
  > EOF

  $ cat >test.ml <<EOF
  > let be =
  >   match Sys.backend_type with
  >   | Native -> "native"
  >   | Bytecode -> "byte"
  >   | Other s -> s
  > let%test "test1" =
  >   print_endline be;
  >   true
  > EOF

  $ dune test
  File "dune", line 4, characters 1-28:
  4 |  (inline_tests (modes byte))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  You are doing something unexpected with the tests. No tests have 
  been run. You should use the inline_tests_runner script to run 
  tests.
  [1]

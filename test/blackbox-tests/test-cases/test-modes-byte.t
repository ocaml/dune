Dune should execute inline tests whose only mode is `byte`.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name test_bytecode_repro)
  >  (inline_tests)
  >  (preprocess (pps ppx_inline_test))
  >  (modes byte))
  > EOF

  $ cat > test_bytecode_repro.ml <<EOF
  >  let rec fact n = if n = 1 then 1 else n * fact (n - 1)
  >  let%test _ = fact 5 = 121
  > EOF

The test should fail.

  $ dune runtest
  File "test_bytecode_repro.ml", line 2, characters 1-26: <<(fact 5) = 121>> is false.
  
  FAILED 1 / 1 tests
  [1]

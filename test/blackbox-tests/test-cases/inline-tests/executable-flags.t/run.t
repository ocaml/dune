This test ensures that compilation fails when an invalid option is supplied
to flags field in executable field in inline_tests field.

First, we pass a valid option to flags field expecting compilation
to be successful.

  $ dune runtest valid_options --root ./test-project
  Entering directory 'test-project'
  backend_foo
  Leaving directory 'test-project'

Lastly, we pass an invalid option to flags field expecting compilation
to fail.

  $ output=$(dune runtest invalid_options --root ./test-project 2>&1); result=$?; (echo $output | grep -o "unknown option '-option-that-is-not-accepted-by-ocaml'."); (exit $result)
  unknown option '-option-that-is-not-accepted-by-ocaml'.
  [1]

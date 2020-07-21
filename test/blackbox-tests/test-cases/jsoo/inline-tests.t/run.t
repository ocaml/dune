Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ dune runtest
  inline_test_runner_inline_tests_byte alias byte/runtest
  inline tests (Byte)
  inline tests (Byte)
          node alias js/runtest
  inline tests (JS)
  inline tests (JS)
  inline_test_runner_inline_tests_native alias native/runtest
  inline tests (Native)
  inline tests (Native)

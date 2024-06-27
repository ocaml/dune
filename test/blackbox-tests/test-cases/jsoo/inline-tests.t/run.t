Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (Native)
  inline tests (Native)
  inline tests (JS)
  inline tests (JS)

  $ dune runtest --profile release
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)

  $ dune build js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc --display short
  Error: Don't know how to build
  js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc
  [1]

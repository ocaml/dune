Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)
  inline tests (JS + custom runner)
  inline tests (JS + custom runner)
  Preparing node wrapper for ../../../.inline_tests_js2.inline-tests/inline_test_runner_inline_tests_js2.bc.js

  $ dune build bin/node_wrapper.exe

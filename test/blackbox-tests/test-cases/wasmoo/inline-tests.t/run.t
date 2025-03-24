Run inline tests using Node.js

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (Native)
  inline tests (Native)
  inline tests (Wasm)
  inline tests (Wasm)

  $ dune runtest --profile release
  inline tests (Native)
  inline tests (Native)
  inline tests (Wasm)
  inline tests (Wasm)

  $ dune build wasm/.inline_tests_wasm.inline-tests/inline_test_runner_inline_tests_wasm.bc --display short
  Error: Don't know how to build
  wasm/.inline_tests_wasm.inline-tests/inline_test_runner_inline_tests_wasm.bc
  [1]

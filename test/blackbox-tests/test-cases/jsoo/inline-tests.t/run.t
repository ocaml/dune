Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

(With the dev profile on OCaml 5, the warning is expected)

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (Byte - alias)
  inline tests (Byte - alias)
  inline tests (Native)
  inline tests (Native)
  Warning [missing-effects-backend]: your program contains effect handlers; you should probably run js_of_ocaml with option '--effects=cps'
  inline tests (JS)
  inline tests (JS)

CR-Alizter: This test has a different behaviour for the macos-latest in the CI and additionally runs the (Byte) tests. This seems unintentional and should be investigated. For now this test is disabled on macos.

  $ dune runtest --profile release
  Warning [missing-effects-backend]: your program contains effect handlers; you should probably run js_of_ocaml with option '--effects=cps'
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)

  $ dune build js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc
  Error: Don't know how to build
  js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc
  [1]

  $ dune build @runtest-js
  Warning [missing-effects-backend]: your program contains effect handlers; you should probably run js_of_ocaml with option '--effects=cps'
  inline tests (JS - alias)
  inline tests (JS - alias)

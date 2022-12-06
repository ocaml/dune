Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)

  $ dune runtest --profile release
  inline tests (Native)
  inline tests (Native)
  File "js/dune", line 1, characters 0-123:
  1 | (library
  2 |  (name inline_tests_js)
  3 |  (js_of_ocaml (javascript_files js.js))
  4 |  (inline_tests (modes js) (backend fake_backend)))
  Error: No rule found for
  js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc-for-jsoo
  [1]

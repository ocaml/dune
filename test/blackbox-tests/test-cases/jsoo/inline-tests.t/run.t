Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  inline tests (Native)
  inline tests (Native)
  File "js/.inline_tests_js.inline-tests/_unknown_", line 1, characters 0-0:
  Error: No rule found for js/.inline_tests_js.inline-tests/js.js
  [1]

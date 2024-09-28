Run inline tests using node js

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

(With the dev profile on OCaml 5, the warning is expected)

  $ dune runtest
  inline tests (Byte)
  inline tests (Byte)
  Warning: your program contains effect handlers; you should probably run js_of_ocaml with option '--enable=effects'
  inline tests (Native)
  inline tests (Native)
  inline tests (JS)
  inline tests (JS)

  $ dune runtest --profile release
  Warning: your program contains effect handlers; you should probably run js_of_ocaml with option '--enable=effects'
  inline tests (JS)
  inline tests (JS)
  inline tests (Native)
  inline tests (Native)

  $ dune build js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc --display short
  Error: Don't know how to build
  js/.inline_tests_js.inline-tests/inline_test_runner_inline_tests_js.bc
  [1]

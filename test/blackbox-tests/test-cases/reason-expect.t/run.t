Testing the interaction of reason syntax and ppx expect

  $ cat > dune << EOF
  > (library
  >  (name test)
  >  (inline_tests)
  >  (preprocess (pps ppx_expect)))
  > EOF

  $ cat > test.re << EOF
  > let%expect_test _ = {
  >   print_endline("Hello, world!");
  >   [%expect {| |}]
  > }
  > EOF

In https://github.com/ocaml/dune/issues/7930 there is an issue where reason
syntax and ppx_expect break at dune lang 3.3.

When dune lang is 3.2 everything should work as expected:

  $ cat > dune-project << EOF
  > (lang dune 3.2)
  > EOF

  $ dune test
  File "test.re", line 1, characters 0-0:
  Error: Files _build/default/test.re and _build/default/test.re.corrected
  differ.
  [1]

However on dune lang 3.3 this appears to break:

  $ cat > dune-project << EOF
  > (lang dune 3.3)
  > EOF

  $ dune test
  File "test.re", line 1:
  Error: I/O error: test.re: No such file or directory
  [1]

This appears to only happen for .re files:

  $ rm test.re

  $ cat > test.ml << EOF
  > let%expect_test _ =
  >   print_endline "Hello, world!";
  >   [%expect {| |}]
  > EOF

  $ dune test
  File "test.ml", line 1, characters 0-0:
  Error: Files _build/default/test.ml and _build/default/test.ml.corrected
  differ.
  [1]

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
syntax and ppx_expect break at dune lang 3.3 due to the sandboxing of ppx.

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > EOF

  $ dune test
  File "test.re", line 1, characters 0-0:
  Error: Files _build/default/test.re and _build/default/test.re.corrected
  differ.
  [1]

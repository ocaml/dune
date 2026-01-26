Testing the interaction of reason syntax and ppx expect

  $ cat > dune << EOF
  > (library
  >  (name test)
  >  (inline_tests)
  >  (preprocess (pps ppx_expect)))
  > EOF

  $ cat > test.re << EOF
  > let%expect_test _ =
  >   print_endline("Hello, world!");
  >   [%expect {| |}]
  > EOF

In https://github.com/ocaml/dune/issues/7930 there is an issue where reason
syntax and ppx_expect break at dune lang 3.3 due to the sandboxing of ppx.

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > EOF
  $ dune test 2>&1 | head -n 6
  File "dune", line 3, characters 1-15:
  3 |  (inline_tests)
       ^^^^^^^^^^^^^^
  Fatal error: exception Invalid_argument("pos + len past end: 0 + 79 > 72")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Base__Ordered_collection_common0.check_pos_len_exn in file "src/ordered_collection_common0.ml" (inlined), line 31, characters 7-53
  [1]

No ppx driver found

  $ echo '(lang dune 2.8)' > dune-project
  $ cat >dune <<EOF
  > (library
  >  (name foo1)
  >  (public_name foo.1)
  >  (modules foo1)
  >  (preprocess (pps)))
  > EOF
  $ dune build
  File "dune", line 5, characters 13-18:
  5 |  (preprocess (pps)))
                   ^^^^^
  Error: You must specify at least one ppx rewriter.
  [1]

Running `dune subst` should succeed in a directory containing just a `.opam` file.
Regression test for https://github.com/ocaml/dune/issues/11290

  $ cat > foo.opam << EOF
  > authors: [ "John Doe <john@doe.com>" ]
  > EOF

  $ dune subst

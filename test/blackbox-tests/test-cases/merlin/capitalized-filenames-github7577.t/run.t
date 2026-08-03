We check that the Merlin helper can handle filenames with capital letters in them.

  $ make_dune_project 3.7

  $ touch mainFOO.ml

  $ cat >dune <<EOF
  > (executable (name mainFOO))
  > EOF

  $ dune build

  $ query_ocaml_merlin_pp mainFOO.ml | grep ERROR
  [1]

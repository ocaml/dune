We check that the Merlin helper can handle filenames with capital letters in them.

  $ make_dune_project 3.7

  $ touch mainFOO.ml

  $ cat >dune <<EOF
  > (executable (name mainFOO))
  > EOF

  $ dune build

  $ printf '(4:File10:mainFOO.ml)4:Halt' | dune ocaml merlin start-session | grep ERROR
  [1]

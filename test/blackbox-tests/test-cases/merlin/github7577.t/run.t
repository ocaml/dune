We check that the Merlin helper can handle filenames with capital letters in them.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ touch mainFOO.ml

  $ cat >dune <<EOF
  > (executable (name mainFOO))
  > EOF

  $ dune build

  $ echo -n '(4:File10:mainFOO.ml)4:Halt' | dune ocaml merlin start-session | grep ERROR
  ((5:ERROR62:No config found for file mainfoo.ml. Try calling `dune build`.))

This test demonstrates that -ppx is missing when two stanzas are in the same
dune file, but require different ppx specifications

  $ dune build @all --profile release
  $ cat .merlin | grep "^FLG"
  FLG -open Usesppx1 -w -40 -open Usesppx2 -w -40

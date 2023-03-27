Dune uses the host context to look up dependencies and build PPXes

  $ dune build

PPX is only built in the host context

  $ ls _build/cross-environment/ppx
  dune
  fooppx.ml
  $ ls _build/default/ppx
  dune
  fooppx.a
  fooppx.cma
  fooppx.cmxa
  fooppx.cmxs
  fooppx.ml

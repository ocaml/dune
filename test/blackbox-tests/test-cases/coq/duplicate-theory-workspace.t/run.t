Duplicate theories in the same project should be caught by Dune:

  $ cat > A/dune << EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ cat > B/dune << EOF
  > (coq.theory
  >  (name foo))
  > EOF

BUG this is not caught

  $ dune build

  $ ls _build/default/A _build/default/B
  _build/default/A:
  Nfoo_a.cmi
  Nfoo_a.cmx
  Nfoo_a.cmxs
  Nfoo_a.o
  a.glob
  a.v
  a.vo
  a.vok
  a.vos
  
  _build/default/B:
  Nfoo_a.cmi
  Nfoo_a.cmx
  Nfoo_a.cmxs
  Nfoo_a.o
  a.glob
  a.v
  a.vo
  a.vok
  a.vos

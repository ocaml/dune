Testing the Load command in coqc and how dune is able to support its
dependencies.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using coq 0.9)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (extra_sources a.v)
  >  (modules :standard \ a))
  > EOF

  $ cat > a.v
  $ cat > b.v <<EOF
  > Load a.
  > EOF

  $ dune build b.vo

  $ ls _build/default/
  Nfoo_b.cmi
  Nfoo_b.cmx
  Nfoo_b.cmxs
  Nfoo_b.o
  a.v
  b.glob
  b.v
  b.vo
  b.vok
  b.vos

Dune should have compiled b.vo:

  $ [ -e _build/default/b.vo ]

Making sure that a.vo doesn't exist:
 
  $ [ -n _build/default/a.vo ]

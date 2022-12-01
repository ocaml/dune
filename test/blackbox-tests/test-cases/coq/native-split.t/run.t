Testing split compilation of native

  $ cat > a.v << EOF
  > Inductive a := A.
  > EOF

  $ cat > b.v << EOF
  > Require Import a.
  > Inductive b := B.
  > EOF

  $ cat > c.v << EOF
  > Require Import b.
  > Inductive c := C.
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using coq 0.7)
  > EOF

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (mode native-split)
  > )
  > EOF

  $ dune build --display=short --always-show-command-line
        coqdep a.v.d
        coqdep b.v.d
        coqdep c.v.d
          coqc a.{glob,vo}
     coqnative Nfoo_a.{cmi,cmxs}
          coqc b.{glob,vo}
     coqnative Nfoo_b.{cmi,cmxs}
          coqc c.{glob,vo}
     coqnative Nfoo_c.{cmi,cmxs}

  $ ls -a _build/default/
  .
  ..
  .a.aux
  .b.aux
  .c.aux
  .dune
  Nfoo_a.cmi
  Nfoo_a.cmx
  Nfoo_a.cmxs
  Nfoo_a.o
  Nfoo_b.cmi
  Nfoo_b.cmx
  Nfoo_b.cmxs
  Nfoo_b.o
  Nfoo_c.cmi
  Nfoo_c.cmx
  Nfoo_c.cmxs
  Nfoo_c.o
  a.glob
  a.v
  a.v.d
  a.vo
  a.vok
  a.vos
  b.glob
  b.v
  b.v.d
  b.vo
  b.vok
  b.vos
  c.glob
  c.v
  c.v.d
  c.vo
  c.vok
  c.vos

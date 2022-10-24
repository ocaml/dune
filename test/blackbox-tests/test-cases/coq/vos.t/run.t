Testing vos compilation in Coq

  $ cat > a.v << EOF
  > Inductive t := .
  > EOF

  $ cat > b.v << EOF
  > Require a.
  > EOF

  $ cat > c.v << EOF
  > Require b.
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.6)
  > (using coq 0.7)
  > EOF

  $ cat > dune << EOF
  > (coq.theory
  >  (name th))
  > EOF

  $ ls
  a.v
  b.v
  c.v
  dune
  dune-project

$ coqc a.v -vos

$ ls

  $ dune build c.vos --display=short
        coqdep c.v.d
        coqdep b.v.d
        coqdep a.v.d
          coqc a.vos
          coqc b.vos
          coqc c.vos

  $ ls _build/default
  a.v
  a.v.d
  a.vos
  b.v
  b.v.d
  b.vos
  c.v
  c.v.d
  c.vos

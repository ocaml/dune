  $ mkdir b
  $ cat > b/dune <<EOF
  > (rocq.theory
  >  (name b)
  >  (theories a)
  >  (package csimple))
  > EOF
  $ cat > b/b.v <<EOF
  > From a Require Import a.
  > Definition bar := a.foo.
  > EOF
  $ cat > b/d.v <<EOF
  > From a Require Import a.
  > Definition doo := a.foo.
  > EOF
  $ dune build --display short --debug-dependency-path
        coqdep a/.a.theory.d
        coqdep b/.b.theory.d
          coqc a/Na_a.{cmi,cmxs},a/a.{glob,vo}
          coqc b/Nb_b.{cmi,cmxs},b/b.{glob,vo}
          coqc b/Nb_d.{cmi,cmxs},b/d.{glob,vo}
  $ cat > b/b.v <<EOF
  > From a Require Import a.
  > Definition bar := a.foo.
  > Definition zoo := 4.
  > EOF
  $ dune build --display short --debug-dependency-path
        coqdep b/.b.theory.d
          coqc b/Nb_b.{cmi,cmxs},b/b.{glob,vo}

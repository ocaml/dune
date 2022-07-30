  $ mkdir b
  $ cat > b/dune <<EOF
  > (coq.theory
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
        coqdep a/a.v.d
        coqdep b/b.v.d
        coqdep b/d.v.d
          coqc a/a.{glob,vo}
          coqc b/b.{glob,vo}
          coqc b/d.{glob,vo}
  $ cat > b/b.v <<EOF
  > From a Require Import a.
  > Definition bar := a.foo.
  > Definition zoo := 4.
  > EOF
  $ dune build --display short --debug-dependency-path
        coqdep b/b.v.d
          coqc b/b.{glob,vo}

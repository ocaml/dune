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
          coqc a/.a.aux,a/a.{glob,vo}
          coqc b/.b.aux,b/b.{glob,vo}
          coqc b/.d.aux,b/d.{glob,vo}
  $ cat > b/b.v <<EOF
  > From a Require Import a.
  > Definition bar := a.foo.
  > Definition zoo := 4.
  > EOF
  $ dune build --display short --debug-dependency-path
          coqc b/.b.aux,b/b.{glob,vo}

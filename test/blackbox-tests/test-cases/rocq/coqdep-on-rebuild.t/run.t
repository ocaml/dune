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
          rocq a/.a.theory.d
          rocq b/.b.theory.d
          rocq a/a.{glob,vo}
          rocq b/b.{glob,vo}
          rocq b/d.{glob,vo}
  $ cat > b/b.v <<EOF
  > From a Require Import a.
  > Definition bar := a.foo.
  > Definition zoo := 4.
  > EOF
  $ dune build --display short --debug-dependency-path
          rocq b/.b.theory.d
          rocq b/b.{glob,vo}

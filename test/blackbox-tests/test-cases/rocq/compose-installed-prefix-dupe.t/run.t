If we have installed theories A.B and A.C then Dune should not complain that A
is a duplicate theory.

First we install our two theories with the conflicting name prefix.

  $ (cd B && dune build @install && dune install --prefix .)
  $ (cd C && dune build @install && dune install --prefix .)

We add these to COQPATH

  $ export COQPATH=../B/lib/coq/user-contrib:../C/lib/coq/user-contrib:$COQPATH

Now we create a theory that depends on both

  $ mkdir mytheory && cd mytheory

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > dune << EOF
  > (rocq.theory
  >  (name mytheory)
  >  (theories A.B A.C))
  > EOF

  $ cat > a.v << EOF
  > From A.B Require Import a.
  > From A.C Require Import a.
  > Print b.
  > EOF

  $ dune build a.vo
  Inductive b : Prop :=  .
 

  $ echo "(rocq.theory (name bug) (mode vo))" > dune
  $ echo "(lang dune 3.21)" > dune-project
  $ echo "(using rocq 0.11)" >> dune-project
  $ touch root.v leaf.v
  $ dune build
  $ find _build -name "*.vo" | sort
  _build/default/leaf.vo
  _build/default/root.vo
  $ echo "Require Import bug.root." >> leaf.v

This test makes sure that a full rebuild is not triggered when the output of
coqdep is changed.

This is as expected:
  $ dune build --display=short
        coqdep .bug.theory.d
          coqc leaf.{glob,vo}

  $ echo "(coq.theory (name bug))" > dune
  $ echo "(lang dune 3.12)" > dune-project
  $ echo "(using coq 0.8)" >> dune-project
  $ touch root.v leaf.v
  $ dune build
  $ find _build -name "*.vo" | sort
  _build/default/leaf.vo
  _build/default/root.vo
  $ echo "Require Import bug.root." >> leaf.v
  $ dune build --display=short
        coqdep .bug.theory.d
          coqc leaf.{glob,vo}

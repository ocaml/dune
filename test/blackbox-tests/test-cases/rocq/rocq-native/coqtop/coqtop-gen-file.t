Running the Coq Toplevel on a generated file with a dependency.

  $ cat >bar.v <<EOF
  > From basic Require Import foo.
  > Definition mynum (i : mynat) := 3.
  > EOF
  $ cat >foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ make_rocq_project 3.21 0.11
  $ cat >dune <<EOF
  > (rocq.theory
  >  (name basic))
  > (rule
  >  (target bar_gen.v)
  >  (deps bar.v)
  >  (action (copy %{deps} %{target})))
  > EOF
  $ cat bar.v | dune rocq top bar_gen.v > /dev/null 2> /dev/null

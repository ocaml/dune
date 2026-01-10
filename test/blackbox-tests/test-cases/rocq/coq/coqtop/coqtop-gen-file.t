Running the Coq Toplevel on a generated file with a dependency.

  $ cat >bar.v <<EOF
  > From basic Require Import foo.
  > Definition mynum (i : mynat) := 3.
  > EOF
  $ cat >foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using coq 0.8)
  > EOF
  $ cat >dune <<EOF
  > (coq.theory
  >  (name basic))
  > (rule
  >  (target bar_gen.v)
  >  (deps bar.v)
  >  (action (copy %{deps} %{target})))
  > EOF
  $ cat bar.v | dune coq top bar_gen.v > /dev/null 2> /dev/null

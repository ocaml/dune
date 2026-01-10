Running the Coq Toplevel on a file with a dependency.

  $ cat >bar.v <<EOF
  > From basic Require Import foo.
  > Definition mynum (i : mynat) := 3.
  > EOF
  $ cat >foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF
  $ cat >dune <<EOF
  > (rocq.theory
  >  (name basic))
  > EOF
  $ cat bar.v | dune rocq top bar.v > /dev/null 2> /dev/null

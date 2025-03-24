From basic Require Import foo.

Definition mynum (i : mynat) := 3.

(* This should not emit a warning *)
Variable (A : nat).

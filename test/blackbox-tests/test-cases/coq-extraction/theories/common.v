
Require Import Ascii.

Axiom axiom : Prop.
Axiom axiom_valid : axiom.

Lemma test : forall a b, eqb a b = eqb b a.
Proof.
  intros. destruct (eqb a b) eqn: E.
  - rewrite eqb_eq in E. symmetry. now rewrite eqb_eq.
  - rewrite eqb_neq in E. symmetry. rewrite eqb_neq. now auto.
Qed.

Definition v2 := 18.


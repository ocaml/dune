
Set Implicit Arguments.
Require Export common.

Definition k (A B : Type) (_ : A) (b : B) := b.

Definition v3 := k axiom_valid v2.

Definition v : nat.
Proof.
  eapply k.
  - exact test.
  - exact v3.
Defined.


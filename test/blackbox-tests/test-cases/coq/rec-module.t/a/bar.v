From rec_module Require Import b.foo.
From rec_module Require Import c.ooo.
From rec_module Require c.d.bar.

Definition mynum (i : mynat) := 3 + ooo_nat + c.d.bar.bar_nat.

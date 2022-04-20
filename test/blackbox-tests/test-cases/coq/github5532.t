Reproducing test case for #5532.

  $ cat >foo.v <<EOF
  > Lemma dummy : True.
  > Proof. idtac "A". Qed.
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.1)
  > (using coq 0.3)
  > EOF

  $ cat >dune <<EOF
  > (coq.theory
  >  (name basic))
  > EOF

  $ dune build --display short
        coqdep foo.v.d
  File "_unknown_", line 1, characters 0-0:
          coqc .foo.aux,foo.{glob,vo} (exit 1)
  A
  File "./foo.v", line 2, characters 18-22:
  Error:  (in proof dummy): Attempt to save an incomplete proof
  
  [1]

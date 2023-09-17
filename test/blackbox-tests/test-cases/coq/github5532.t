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

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  File "dune", line 1, characters 0-26:
  1 | (coq.theory
  2 |  (name basic))
  A
  File "./foo.v", line 2, characters 18-22:
  Error:  (in proof dummy): Attempt to save an incomplete proof
  
  [1]

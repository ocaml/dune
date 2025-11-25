Reproducing test case for #5532.

  $ cat >foo.v <<EOF
  > Lemma dummy : True.
  > Proof. idtac "A". Qed.
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat >dune <<EOF
  > (rocq.theory
  >  (name basic))
  > EOF

  $ dune build
  File "dune", lines 1-2, characters 0-27:
  1 | (rocq.theory
  2 |  (name basic))
  A
  File "./foo.v", line 2, characters 18-22:
  Error:  (in proof dummy): Attempt to save an incomplete proof
  (there are remaining open goals).
  
  [1]

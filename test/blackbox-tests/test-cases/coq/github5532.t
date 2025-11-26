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
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  File "dune", lines 1-2, characters 0-26:
  1 | (coq.theory
  2 |  (name basic))
  A
  File "./foo.v", line 2, characters 18-22:
  Error:  (in proof dummy): Attempt to save an incomplete proof
  
  [1]

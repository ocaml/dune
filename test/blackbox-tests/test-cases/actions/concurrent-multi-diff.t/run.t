The use of (concurrent ) illustrated in the context of diffing multiple files.

Say we want to diff 3 files.
  $ cat A
  I am file A.
  $ cat B
  I am file B.
  $ cat C
  I am file C.

We set up a (progn ) rule to diff all of them against their generated versions.

  $ cat > dune << EOF
  > (rule
  >  (action
  >   (progn
  >    (with-outputs-to A.diff (echo "I am file A.\n"))
  >    (with-outputs-to B.diff (echo "I am certainly file B.\n"))
  >    (with-outputs-to C.diff (echo "I am most certainly file C.\n")))))
  > 
  > (rule
  >  (action
  >   (progn
  >    (with-outputs-to some-target (echo a))
  >    (diff A A.diff)
  >    (diff B B.diff)
  >    (diff C C.diff))))
  > EOF

We can now run the rule and see that we fail before diffing C.

  $ dune build
  File "B", line 1, characters 0-0:
  Error: Files _build/default/B and _build/default/B.diff differ.
  [1]

We can check which diffs were run by asking Dune to promote the files.

  $ dune promotion apply
  Promoting _build/default/B.diff to B.

Since we failed early, only B was promoted.

Let's reset B to its original state.

  $ rm B
  $ cat > B << EOF
  > I am file B.
  > EOF

If we implement the rule using (concurrent ) instead.

  $ cat > dune << EOF
  > (rule
  >  (action
  >   (progn
  >    (with-outputs-to A.diff (echo "I am file A.\n"))
  >    (with-outputs-to B.diff (echo "I am certainly file B.\n"))
  >    (with-outputs-to C.diff (echo "I am most certainly file C.\n")))))
  > 
  > (rule
  >  (action
  >   (concurrent
  >    (with-outputs-to some-target (echo a))
  >    (diff A A.diff)
  >    (diff B B.diff)
  >    (diff C C.diff))))
  > EOF

We see that all the files get diffed.

  $ dune build
  File "B", line 1, characters 0-0:
  Error: Files _build/default/B and _build/default/B.diff differ.
  File "C", line 1, characters 0-0:
  Error: Files _build/default/C and _build/default/C.diff differ.
  [1]

And we have promotions for the two that failed.

  $ dune promote
  Promoting _build/default/B.diff to B.
  Promoting _build/default/C.diff to C.


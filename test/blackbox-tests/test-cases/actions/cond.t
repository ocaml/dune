Testing (case) action.

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > EOF

Basic usage:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (cond
  >    ((= 1 2) (echo " 1 is 2"))
  >    ((= 2 3) (echo "2 is 3"))
  >    ((< 3 4) (echo "3 is less than 4"))
  >    (_ (echo "shouldn't happen")))))
  > EOF

  $ dune build @foo
  3 is less than 4

Missing default field:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (cond
  >    ((= 1 2) (echo " 1 is 2")))))
  > EOF

  $ dune build @foo
  File "dune", line 5, characters 4-11:
  5 |    ((= 1 2) (echo " 1 is 2")))))
          ^^^^^^^
  Error: The final branch must be the default one.
  Hint: Add a (_ (...)) case at the end.
  [1]

Two default fields:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version}
  >    (_ (echo "version is 1"))
  >    (2 (echo "version is 2"))
  >    (_ (echo "version is 3")))))
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (cond
  >    (_ (echo " 1 is 2"))
  >    ((= 2 3) (echo "2 is 3"))
  >    (_ (echo "3 is less than 4")))))
  > EOF

  $ dune build @foo
  Error: Multiple default cases.
  File "dune", line 5, characters 4-5:
  5 |    (_ (echo " 1 is 2"))
          ^
  
  File "dune", line 7, characters 4-5:
  7 |    (_ (echo "3 is less than 4")))))
          ^
  
  [1]

Last case is not default case:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (cond
  >    ((= 1 2) (echo " 1 is 2"))
  >    ((= 2 3) (echo "2 is 3"))
  >    (_ (echo "shouldn't happen"))
  >    ((< 3 4) (echo "3 is less than 4")))))
  > EOF

  $ dune build @foo
  File "dune", line 8, characters 4-11:
  8 |    ((< 3 4) (echo "3 is less than 4")))))
          ^^^^^^^
  Error: The final branch must be the default one.
  Hint: Add a (_ (...)) case at the end.
  [1]

Empty case:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (cond)))
  > EOF

  $ dune build @foo
  File "dune", line 4, characters 2-8:
  4 |   (cond)))
        ^^^^^^
  Error: Not enough arguments for cond
  [1]

Duplicate cases:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (cond
  >    ((= 1 1) (echo "1 is 1"))
  >    ((= 1 1) (echo "1 is 1"))
  >    (_ (echo "shouldn't happen")))))
  > EOF

  $ dune build @foo
  Error: Duplicate case.
  File "dune", line 5, characters 4-11:
  5 |    ((= 1 1) (echo "1 is 1"))
          ^^^^^^^
  
  File "dune", line 6, characters 4-11:
  6 |    ((= 1 1) (echo "1 is 1"))
          ^^^^^^^
  
  [1]

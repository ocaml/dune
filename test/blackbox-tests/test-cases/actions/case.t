Testing (case) action.

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > EOF

  $ cat > version << EOF
  > 1
  > EOF

Basic usage:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version}
  >    (1 (echo "version is 1"))
  >    (2 (echo "version is 2"))
  >    (3 (echo "version is 3"))
  >    (_ (echo "shouldn't happen, got %{read-lines:version}")))))
  > EOF

  $ dune build @foo
  version is 1

  $ cat > version << EOF
  > 2
  > EOF
  $ dune build @foo
  version is 2

  $ cat > version << EOF
  > 3
  > EOF
  $ dune build @foo
  version is 3

  $ cat > version << EOF
  > 4
  > EOF
  $ dune build @foo
  shouldn't happen, got 4

  $ cat > version << EOF
  > 1
  > EOF

Missing default field:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version}
  >    (1 (echo "version is 1")))))
  > EOF

  $ dune build @foo
  File "dune", line 5, characters 4-5:
  5 |    (1 (echo "version is 1")))))
          ^
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

  $ dune build @foo
  Error: Multiple default cases.
  File "dune", line 5, characters 4-5:
  5 |    (_ (echo "version is 1"))
          ^
  
  File "dune", line 7, characters 4-5:
  7 |    (_ (echo "version is 3")))))
          ^
  
  [1]

Last case is not default case:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version}
  >    (_ (echo "version is 1"))
  >    (2 (echo "version is 2"))
  >    (3 (echo "version is 3")))))
  > EOF

  $ dune build @foo
  File "dune", line 7, characters 4-5:
  7 |    (3 (echo "version is 3")))))
          ^
  Error: The final branch must be the default one.
  Hint: Add a (_ (...)) case at the end.
  [1]

Empty case:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version})))
  > EOF

  $ dune build @foo
  File "dune", line 4, characters 2-30:
  4 |   (case %{read-lines:version})))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Not enough arguments for case
  [1]

Duplicate cases:

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version}
  >    (1 (echo "version is 1"))
  >    (1 (echo "version is 1"))
  >    (_ (echo "shouldn't happen")))))
  > EOF

  $ dune build @foo
  Error: Duplicate case.
  File "dune", line 5, characters 4-5:
  5 |    (1 (echo "version is 1"))
          ^
  
  File "dune", line 6, characters 4-5:
  6 |    (1 (echo "version is 1"))
          ^
  
  [1]

Demo what happens when "_" is used as a pattern.

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (case %{read-lines:version}
  >    ("_" (echo "version is _"))
  >    (_ (echo "version is default")))))
  > EOF

  $ dune build @foo
  version is default

  $ cat > version << EOF
  > _
  > EOF

  $ dune build @foo
  version is _

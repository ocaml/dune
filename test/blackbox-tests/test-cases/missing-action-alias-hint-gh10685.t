  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias child_one)
  >  (action (echo "Child one")))
  > 
  > (rule
  >  (alias child_two)
  >  (action (echo "Child two")))
  > 
  > (rule
  >  (alias parent)
  >  (deps
  >   (alias child_one)
  >   (alias child_two)))
  > EOF

  $ dune build
  File "dune", lines 9-13, characters 0-70:
   9 | (rule
  10 |  (alias parent)
  11 |  (deps
  12 |   (alias child_one)
  13 |   (alias child_two)))
  Error: Field "action" is missing
  Hint: You can use the (alias) stanza to add dependencies to an alias.
  [1]

  $ cat > dune << EOF
  > (rule
  >  (alias child_one)
  >  (action (echo "Child one")))
  > 
  > (rule
  >  (alias child_two)
  >  (action (echo "Child two")))
  > 
  > (rule
  >  (deps
  >   (alias child_one)
  >   (alias child_two)))
  > EOF

  $ dune build
  File "dune", lines 9-12, characters 0-54:
   9 | (rule
  10 |  (deps
  11 |   (alias child_one)
  12 |   (alias child_two)))
  Error: Field "action" is missing
  [1]

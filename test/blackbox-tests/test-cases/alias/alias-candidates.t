Dune should suggest similar aliases when it cannot find one. 

  $ make_dune_project 3.7
  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (echo "Hello, world from \"foo\"!")))
  > EOF

We have an alias "foo" but let's try to build something misspeled:
  $ dune build @fou
  Error: Alias "fou" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  Hint: did you mean fmt or foo?
  [1]

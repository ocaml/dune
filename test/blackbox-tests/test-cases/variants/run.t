Using variant features without enabling isn't allowed

  $ dune build --root variants-without-using
  Entering directory 'variants-without-using'
  File "dune", line 1, characters 0-54:
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation foobar)
  
  This will become an error in the future.

When a rule target can be be inferred from the rule action, the target or targets field can be omitted.
This works with the short form of the rule stanza:

  $ dune build @infer --root short-form
  Entering directory 'short-form'
  It worked!

But should work with the long form as well:

  $ dune build @infer --root long-form
  Entering directory 'long-form'
  File "dune", line 1, characters 0-57:
  1 | (rule
  2 |  (action (write-file b "It should work as well!")))
  Error: fields targets, target are all missing (exactly one is needed)
  [1]

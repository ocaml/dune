When a rule target can be be inferred from the rule action, the target or targets field can be omitted.
This works with the short form of the rule stanza:

  $ dune build @infer --root short-form
  Entering directory 'short-form'
  It worked!

But should work with the long form as well:

  $ dune build @infer --root long-form
  Entering directory 'long-form'
  It should work as well!

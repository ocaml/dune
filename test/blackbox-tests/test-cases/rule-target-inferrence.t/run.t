When a rule target can be be inferred from the rule action, the target or targets field can be omitted.
This works with the short form of the rule stanza:

  $ dune build @infer --root short-form
  Entering directory 'short-form'
  It worked!

But should work with the long form as well:

  $ dune build @infer --root long-form
  Entering directory 'long-form'
  It should work as well!

When an action has no targets, an helpful error message is displayed:

  $ dune build --root no-target
  Entering directory 'no-target'
  File "dune", line 1, characters 0-34:
  1 | (rule (action (echo "something")))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule has no targets specified
  [1]

When the rule action targets cannot be inferred by dune, we should make it explicit
in the error message:

  $ dune build --root cannot-infer
  Entering directory 'cannot-infer'
  File "dune", line 1, characters 0-36:
  1 | (rule (action (system "something")))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule has no targets specified
  [1]

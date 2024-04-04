We should not be able to produce a directory in a rule that already exists

  $ dune build
  File "dune", lines 5-7, characters 0-51:
  5 | (rule
  6 |  (targets dir)
  7 |  (action (run ./foo.exe dir)))
  Error: This rule defines a target "dir" whose name conflicts with a source
  directory in the same directory.
  Hint: If you want Dune to generate and replace "dir", add (mode promote) to
  the rule stanza. Alternatively, you can delete "dir" from the source tree or
  change the rule to generate a different target.
  [1]


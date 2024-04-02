File targets overlapping with directories

  $ dune build @all
  File "dune", lines 1-3, characters 0-57:
  1 | (rule
  2 |  (targets dir)
  3 |  (action (bash "mkdir %{targets}")))
  Error: This rule defines a target "dir" whose name conflicts with a source
  directory in the same directory.
  Hint: If you want Dune to generate and replace "dir", add (mode promote) to
  the rule stanza. Alternatively, you can delete "dir" from the source tree or
  change the rule to generate a different target.
  [1]


  $ dune build --root target && cat target/_build/default/dir/*
  Entering directory 'target'
  bar contents
  foo contents

  $ dune build --root target @cat_dir
  Entering directory 'target'
  bar:
  bar contents
  
  foo:
  foo contents
  

  $ dune build --root dep
  Entering directory 'dep'
  Error: No rule found for dir
  -> required by alias default in dune:1
  [1]

We should not be able to produce a directory in a rule that already exists
  $ dune build --display=short --root no-overlapping-rules
  Entering directory 'no-overlapping-rules'
  File "dune", line 5, characters 0-51:
  5 | (rule
  6 |  (targets dir)
  7 |  (action (run ./foo.exe dir)))
  Error: This rule defines a target "dir" whose name conflicts with a source
  directory in the same directory.
  Hint: If you want Dune to generate and replace "dir", add (mode promote) to
  the rule stanza. Alternatively, you can delete "dir" from the source tree or
  change the rule to generate a different target.
  [1]

Dune crashes if there's a file named after the directory target
  $ dune build --root file-overlap-dir @all
  Entering directory 'file-overlap-dir'
  File "dune", line 1, characters 0-57:
  1 | (rule
  2 |  (targets dir)
  3 |  (action (bash "mkdir %{targets}")))
  Error: This rule defines a target "dir" whose name conflicts with a source
  directory in the same directory.
  Hint: If you want Dune to generate and replace "dir", add (mode promote) to
  the rule stanza. Alternatively, you can delete "dir" from the source tree or
  change the rule to generate a different target.
  [1]

directory target and (mode promote) results in a crash
  $ dune build --root mode-promote @all 2>&1 | head -n2
  Entering directory 'mode-promote'
  Error: Is a directory

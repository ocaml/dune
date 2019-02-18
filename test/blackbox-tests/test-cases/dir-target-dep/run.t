  $ dune build --root target && cat target/_build/default/dir/*
  Entering directory 'target'
  bar contents
  foo contents

  $ dune build --root dep
  Entering directory 'dep'
  File "dune", line 1, characters 0-68:
  1 | (alias
  2 |  (name default)
  3 |  (deps dir)
  4 |  (action (bash "cat %{deps}/*")))
  Error: No rule found for dir
  [1]

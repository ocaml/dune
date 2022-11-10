The current directory cannot be marked as vendored

  $ dune build
  File "dune", line 1, characters 15-16:
  1 | (vendored_dirs .)
                     ^
  Error: invalid sub-directory name "."
  Hint: did you mean (vendored_dirs *)?
  [1]

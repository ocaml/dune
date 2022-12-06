The current directory cannot be marked as data-only

  $ dune build
  File "dune", line 1, characters 16-17:
  1 | (data_only_dirs .)
                      ^
  Error: invalid sub-directory name "."
  Hint: did you mean (data_only_dirs *)?
  [1]

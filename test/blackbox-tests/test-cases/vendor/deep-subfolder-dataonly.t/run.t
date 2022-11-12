Only direct subdirectories can be marked as data-only

  $ dune build
  File "dune", line 1, characters 16-21:
  1 | (data_only_dirs a/b/c)
                      ^^^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore a/b/c, write "(data_only_dirs c)" in a/b/dune
  [1]

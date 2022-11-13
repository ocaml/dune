Only direct subdirectories can be marked as vendored

  $ dune build
  File "dune", line 1, characters 15-18:
  1 | (vendored_dirs a/b)
                     ^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore a/b, write "(vendored_dirs b)" in a/dune
  [1]


dune uses a versioned file. If the version is missing, then we get an error.

  $ dune build
  File "dune-workspace", line 1, characters 0-19:
  1 | (context (default))
      ^^^^^^^^^^^^^^^^^^^
  Error: Invalid first line, expected: (lang <lang> <version>)
  [1]


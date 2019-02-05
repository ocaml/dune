Define c executable using singular form of stanza
  $ dune build --root singular
  Entering directory 'singular'
  File "dune", line 4, characters 0-49:
  4 | (alias
  5 |  (name default)
  6 |  (action (run ./foo.exe)))
  Error: No rule found for foo.exe
  [1]

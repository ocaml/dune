Basic test that we can use private binaries as public ones
  $ dune build --root private-bin-import
  Entering directory 'private-bin-import'
  File "using-priv/dune", line 7, characters 0-79:
   7 | (alias
   8 |  (name runtest)
   9 |  (action
  10 |   (progn
  11 |    (run priv)
  12 |    (run priv-renamed))))
  Error: No rule found for using-priv/.bin/priv
  File "using-priv/dune", line 7, characters 0-79:
   7 | (alias
   8 |  (name runtest)
   9 |  (action
  10 |   (progn
  11 |    (run priv)
  12 |    (run priv-renamed))))
  Error: No rule found for using-priv/.bin/priv-renamed
  [1]

Override public binary in env
  $ dune build --root override-bins
  Entering directory 'override-bins'
  File "test/dune", line 5, characters 0-43:
  5 | (alias
  6 |  (name runtest)
  7 |  (action (run foo)))
  Error: No rule found for test/.bin/foo
  [1]

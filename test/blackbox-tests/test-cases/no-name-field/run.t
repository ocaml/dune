the name field can be omitted for libraries when public_name is present
  $ dune build --root no-name-lib
  Entering directory 'no-name-lib'

this isn't possible for older syntax <= (1, 0)
  $ dune build --root no-name-lib-syntax-1-0
  File "dune", line 1, characters 0-27:
  Error: name field cannot be omitted before version 1.1
  [1]

executable(s) stanza works the same way

  $ dune build --root no-name-exes
  Entering directory 'no-name-exes'

  $ dune build --root no-name-exes-syntax-1-0
  File "dune", line 1, characters 0-36:
  Error: name field may not be omitted before dune version 1.1
  [1]

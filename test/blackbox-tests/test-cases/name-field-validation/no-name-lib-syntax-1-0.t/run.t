this isn't possible for older syntax <= (1, 0)
  $ dune build 
  File "dune", line 1, characters 22-25:
  1 | (library (public_name foo))
                            ^^^
  Error: name field cannot be omitted before version 1.1 of the dune language
  [1]

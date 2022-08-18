exe: invalid public-name
  $ dune build 
  File "dune", line 2, characters 14-17:
  2 |  (public_name a.b))
                    ^^^
  Error: Module "A.b" doesn't exist.
  [1]


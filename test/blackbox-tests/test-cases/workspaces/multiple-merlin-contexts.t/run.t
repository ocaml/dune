Only a single context may be marked for merlin

  $ dune build
  File "dune-workspace", line 8, characters 1-82:
   8 |  (opam
   9 |   (switch foo-switch)
  10 |   (name foo-name)
  11 |   (profile foo-profile)
  12 |   (merlin)))
  Error: you can only have one context for merlin
  [1]

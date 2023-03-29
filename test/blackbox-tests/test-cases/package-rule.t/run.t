When --only-packages is passed, it runs

  $ dune build --only-packages a @runtest
  File "dune", line 12, characters 3-10:
  12 |   (package a)
          ^^^^^^^
  Error: Unknown action or rule field.
  [1]

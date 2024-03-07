  $ dune build @quoted
  File "dune", line 4, characters 25-26:
  4 |  (action (echo %{read:foo bar.txt})))
                               ^
  Error: The character ' ' is not allowed inside %{...} forms
  [1]

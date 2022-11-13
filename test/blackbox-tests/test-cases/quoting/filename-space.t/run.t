  $ dune build @quoted
  File "dune", line 4, characters 17-18:
  4 |  (action (echo %{read:foo bar.txt})))
                       ^
  Error: This character is not allowed inside %{...} forms
  [1]

This behavior is surprising, we should get an error about the fact
that ${@} is not quoted and doesn't contain exactly 1 element

  $ dune build x
  File "dune", line 3, characters 25-35:
  3 |  (action (with-stdout-to %{targets} (echo foo))))
                               ^^^^^^^^^^
  Error: Variable %{targets} expands to 2 values, however a single value is
  expected here. Please quote this atom.
  [1]

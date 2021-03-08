When an unknown pform is encountered, a sensible message is printed out.

  $ echo '(lang dune 2.0)' > dune-project
  $ echo '(rule (copy %{unknown} x.ml))' > dune
  $ dune build
  File "dune", line 1, characters 12-22:
  1 | (rule (copy %{unknown} x.ml))
                  ^^^^^^^^^^
  Error: Unknown variable %{unknown}
  [1]

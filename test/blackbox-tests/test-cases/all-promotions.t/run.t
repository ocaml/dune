All promotions should be run, whether they share locks or not:
The following should trigger two failures, one for a.expected and one for
b.expected but it is not because of a bug introduced in 2.0.

  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  Error: Files _build/default/a.expected and _build/default/a differ.
  File "b.expected", line 1, characters 0-0:
  Error: Files _build/default/b.expected and _build/default/b differ.
  [1]

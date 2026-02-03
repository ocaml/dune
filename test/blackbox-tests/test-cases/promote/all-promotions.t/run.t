All promotions should be run, whether they share locks or not:
The following should trigger two failures, one for a.expected and one for
b.expected but it is not because of a bug introduced in 2.0.

  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ a
  @@ -1 +1 @@
  -a
  +y
  File "b.expected", line 1, characters 0-0:
  --- b.expected
  +++ b
  @@ -1 +1 @@
  -b
  +z
  [1]

All builtin variables are lower cased in Dune:

  $ dune runtest
  File "dune", line 3, characters 39-46:
  3 |  (action (with-stdout-to %{null} (echo %{MAKE}))))
                                             ^^^^^^^
  Error: %{MAKE} was renamed to '%{make}' in the 1.0 version of the dune
  language
  [1]

(enabled_if) needs a recent (lang dune):

  $ dune runtest
  File "dune", line 2, characters 1-18:
  2 |  (enabled_if true))
       ^^^^^^^^^^^^^^^^^
  Error: 'enabled_if' is only available since version 2.9 of the dune language.
  Please update your dune-project file to have (lang dune 2.9).
  [1]

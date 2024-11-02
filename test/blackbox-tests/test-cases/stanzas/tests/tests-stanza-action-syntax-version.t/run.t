If there is an (action) field defined in a test stanza and dune-project sets
(lang dune v) with v < 1.2, a warning is emitted:

  $ dune runtest
  File "dune", line 3, characters 1-23:
  3 |  (action (run %{test})))
       ^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'action' is only available since version 1.2 of the dune language.
  Please update your dune-project file to have (lang dune 1.2).

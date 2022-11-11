The vendored_dirs stanza is available from version 1.11 of the dune language

  $ dune build
  File "dune", line 1, characters 0-17:
  1 | (vendored_dirs *)
      ^^^^^^^^^^^^^^^^^
  Error: 'vendored_dirs' is only available since version 1.11 of the dune
  language. Please update your dune-project file to have (lang dune 1.11).
  [1]

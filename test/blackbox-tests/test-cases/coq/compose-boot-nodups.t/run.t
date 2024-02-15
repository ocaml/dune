Testing composition with two boot libraries

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  Error: Cannot have more than one boot theory in scope:
  - A at A/dune:1
  - B at B/dune:1
  -> required by alias default
  [1]

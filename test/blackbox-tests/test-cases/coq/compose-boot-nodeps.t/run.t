Testing composition with two boot libraries

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "A/dune", line 4, characters 11-12:
  4 |  (theories B)
                 ^
  Error: (boot) libraries cannot have dependencies
  [1]

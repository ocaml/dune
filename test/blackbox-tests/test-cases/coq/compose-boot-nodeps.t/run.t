Testing composition with two boot libraries

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  File "A/dune", line 4, characters 11-12:
  4 |  (theories B)
                 ^
  Error: (boot) libraries cannot have dependencies
  [1]

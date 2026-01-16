Testing composition with two boot libraries

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Error: Cannot have more than one boot theory in scope:
  - A at A/dune:1
  - B at B/dune:1
  -> required by alias default
  [1]

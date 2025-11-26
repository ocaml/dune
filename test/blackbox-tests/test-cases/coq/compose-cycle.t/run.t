We check cycles are detected
  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Error: Dependency cycle between:
     theory A in A/dune:2
  -> theory B in B/dune:2
  -> theory A in A/dune:2
  -> required by _build/default/A/.A.theory.d
  -> required by alias A/all
  -> required by alias default
  [1]

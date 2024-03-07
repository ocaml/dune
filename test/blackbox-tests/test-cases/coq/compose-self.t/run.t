Composing a theory with itself should cause a cycle
  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  Error: Dependency cycle between:
     theory A in A/dune:2
  -> required by _build/default/A/.A.theory.d
  -> required by alias A/all
  -> required by alias default
  [1]

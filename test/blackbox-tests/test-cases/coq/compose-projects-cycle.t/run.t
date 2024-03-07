Testing composition of theories across a dune workspace with cyclic
dependencies.

  $ dune build A
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  Error: Dependency cycle between:
     theory A in A/dune:2
  -> theory B in B/dune:2
  -> theory C in C/dune:2
  -> theory A in A/dune:2
  -> required by _build/default/A/.A.theory.d
  -> required by alias A/all
  -> required by alias A/default
  [1]

  $ dune build B
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  Error: Dependency cycle between:
     theory B in B/dune:2
  -> theory C in C/dune:2
  -> theory A in A/dune:2
  -> theory B in B/dune:2
  -> required by _build/default/B/.B.theory.d
  -> required by alias B/all
  -> required by alias B/default
  [1]

  $ dune build C
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  Error: Dependency cycle between:
     theory C in C/dune:2
  -> theory A in A/dune:2
  -> theory B in B/dune:2
  -> theory C in C/dune:2
  -> required by _build/default/C/.C.theory.d
  -> required by alias C/all
  -> required by alias C/default
  [1]

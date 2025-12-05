By default the coqdep flags are empty.

  $ cp theories/dune.noflags theories/dune
  $ dune build theories/.basic.theory.d
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

We use non-exising coqdep flags, so compilation fails.

  $ mv dune.disabled dune
  $ dune build theories/.basic.theory.d
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  *** Warning: unknown option --global-flag1
  *** Warning: unknown option --global-flag2

We then add more flags locally to the theory.

  $ rm -f theories/dune
  $ cp theories/dune.flags theories/dune
  $ dune build theories/.basic.theory.d
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  *** Warning: unknown option --global-flag1
  *** Warning: unknown option --global-flag2
  *** Warning: unknown option --local-flag1
  *** Warning: unknown option --local-flag2

Finally we remove the toplevel dune file which sets some flags, but keep the
theory-local flags only.

  $ rm -f dune
  $ dune build theories/.basic.theory.d
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  *** Warning: unknown option --local-flag1
  *** Warning: unknown option --local-flag2

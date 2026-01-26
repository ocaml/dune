In Coq >= 0.6, depending on a private library as a plugin is an error.
  $ cat > dune << EOF
  > (library
  >  (name foo))
  > 
  > (coq.theory
  >  (name bar)
  >  (plugins foo))
  > EOF

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  File "dune", line 6, characters 10-13:
  6 |  (plugins foo))
                ^^^
  Error: Using private library foo as a Coq plugin is not supported
  [1]

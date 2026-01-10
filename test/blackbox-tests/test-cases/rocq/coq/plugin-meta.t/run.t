The META file for plugins is built before calling coqdep
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (coq.theory
  >  (name bar)
  >  (plugins bar.foo))
  > EOF

  $ dune build .bar.theory.d
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ ls _build/install/default/lib/bar
  META


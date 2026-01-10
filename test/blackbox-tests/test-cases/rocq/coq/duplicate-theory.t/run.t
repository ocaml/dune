Duplicate theories should be caught by Dune:

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo))
  > 
  > (coq.theory
  >  (name foo))
  > EOF

BUG The directory target is found before the theory

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Error: Coq theory foo is defined twice:
  - theory foo in dune:5
  - theory foo in dune:2
  [1]

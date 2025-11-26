Duplicate theories in the same project should be caught by Dune:

  $ mkdir A
  $ mkdir B

  $ cat > A/dune << EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ cat > B/dune << EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Error: Coq theory foo is defined twice:
  - theory foo in A/dune:2
  - theory foo in B/dune:2
  -> required by alias default
  [1]

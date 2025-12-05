Testing the _CoqProject generation.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name a)
  >  (modules foo)
  >  (generate_project_file))
  > 
  > (coq.theory
  >  (name b)
  >  (modules bar)
  >  (generate_project_file))
  > EOF

  $ touch foo.v bar.v

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Error: Multiple rules generated for _build/default/_CoqProject:
  - dune:6
  - dune:1
  [1]

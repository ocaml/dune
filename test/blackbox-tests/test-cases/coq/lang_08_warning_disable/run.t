Test that the warning for coq lang 0.8 can be disabled

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (using coq 0.7)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name Foo))
  > EOF

  $ cat > a.v <<EOF
  > Definition a := Type.
  > EOF

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

We get the warning again, even on a zero-build:

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (using coq 0.7)
  > (warnings (deprecated_coq_lang_lt_08 disabled))
  > EOF

  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

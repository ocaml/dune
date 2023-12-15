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
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

We get the warning again, even on a zero-build:

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (using coq 0.7)
  > (warnings (deprecated_coq_lang_lt_08 disabled))
  > EOF

  $ dune build

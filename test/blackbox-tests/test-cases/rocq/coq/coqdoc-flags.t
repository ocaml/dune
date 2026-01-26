Testing the coqdoc flags field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using coq 0.8)
  > EOF

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (coq
  >    (coqdoc_flags :standard -toc-depth 2))))
  > (coq.theory
  >  (name a))
  > EOF
  $ cat > foo.v <<EOF
  > Definition a := 42.
  > EOF

  $ dune build @doc
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

  $ dune trace cat | jq 'include "dune"; coqdocFlags'
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "a"
  "--toc"
  "-toc-depth"
  "2"
  "--html"
  "-d"
  "a.html"
  "foo.v"

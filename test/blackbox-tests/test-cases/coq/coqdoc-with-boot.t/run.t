Testing coqdoc when composed with a boot library

  $ dune build A/A.html
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

  $ ls _build/default/A
  A.html
  a.glob
  a.v
  a.vo
  a.vok
  a.vos

Dune should be passing '--coqlib' to coqdoc, but it doesn't. This is a bug.

  $ dune trace cat | jq -c 'include "dune"; coqdocFlags'
  "-R"
  "../Coq"
  "Coq"
  "-R"
  "."
  "A"
  "--toc"
  "--html"
  "-d"
  "A.html"
  "a.v"

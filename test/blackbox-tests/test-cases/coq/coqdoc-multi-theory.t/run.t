HTML
----

First we build the doc alias for the first theory
  $ dune build @A/doc
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
The first theory doc is built
  $ ls _build/default/A/A.html
  A.AA.aa.html
  A.AB.ab.html
  coqdoc.css
  index.html
  toc.html
Check that the second is not built
  $ ls _build/default/
  A
Clean
  $ dune clean

Next we build the doc for the second theory
  $ dune build @B/doc
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
Check that the first theory doc is not built
  $ ls _build/default/A/
  AA
  AB
Check that the second theory doc is built
  $ ls _build/default/B/B.html
  B.b.html
  coqdoc.css
  index.html
  toc.html
Clean
  $ dune clean


LaTeX
-----

Next we test the LaTeX targets in the same manner
  $ dune build @A/doc-latex
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
The first theory doc is built
  $ ls _build/default/A/A.tex
  A.AA.aa.tex
  A.AB.ab.tex
  coqdoc.sty
Check that the second is not built
  $ ls _build/default
  A
Clean
  $ dune clean

Next we build the doc for the second theory
  $ dune build @B/doc-latex
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
Check that the first theory doc is not built
  $ ls _build/default/A
  AA
  AB
Check that the second theory doc is built
  $ ls _build/default/B/B.tex
  B.b.tex
  coqdoc.sty

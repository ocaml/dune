HTML
----

First we build the doc alias for the first theory
  $ dune build @A/doc
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
Check that the first theory doc is not built
  $ ls _build/default/A/
  A.theory.d
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
Check that the first theory doc is not built
  $ ls _build/default/A
  A.theory.d
  AA
  AB
Check that the second theory doc is built
  $ ls _build/default/B/B.tex
  B.b.tex
  coqdoc.sty

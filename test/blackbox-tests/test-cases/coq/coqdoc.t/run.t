We build the coqdoc html target:
  $ dune build basic.html/
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Now we inspect it:
  $ ls _build/default/basic.html
  basic.bar.html
  basic.foo.html
  coqdoc.css
  index.html
  toc.html

We build the coqdoc latex target:
  $ dune build basic.tex/
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Now we inspect it:
  $ ls _build/default/basic.tex
  basic.bar.tex
  basic.foo.tex
  coqdoc.sty

Next from a clean build we make sure that @all does *not* build any doc targets:
  $ dune clean
  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
Note that this currently works due to a bug in @all detecting directory targets.
  $ ls _build/default
  META.base
  bar.glob
  bar.v
  bar.vo
  bar.vok
  bar.vos
  base.dune-package
  base.install
  foo.glob
  foo.v
  foo.vo
  foo.vok
  foo.vos


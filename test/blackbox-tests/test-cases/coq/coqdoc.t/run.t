We build the coqdoc html target:
  $ dune build basic.html/

Now we inspect it:
  $ ls _build/default/basic.html
  basic.bar.html
  basic.foo.html
  coqdoc.css
  index.html
  toc.html

We build the coqdoc latex target:
  $ dune build basic.tex/

Now we inspect it:
  $ ls _build/default/basic.tex
  basic.bar.tex
  basic.foo.tex
  coqdoc.sty

Next from a clean build we make sure that @all does *not* build any doc targets:
  $ dune clean
  $ dune build @all
Note that this currently works due to a bug in @all detecting directory targets.
  $ ls _build/default
  META.base
  bar.glob
  bar.v
  bar.v.d
  bar.vo
  bar.vok
  bar.vos
  base.dune-package
  base.install
  foo.glob
  foo.v
  foo.v.d
  foo.vo
  foo.vok
  foo.vos


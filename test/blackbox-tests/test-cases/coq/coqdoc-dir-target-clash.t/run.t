We try to build the documentation but there will be a clash between the
directory targets. Notice how the tex one fails before html one.
  $ dune build @check
  File "dune", line 9, characters 0-116:
   9 | (rule
  10 |  (targets
  11 |   (dir base.tex))
  12 |  (action
  13 |   (progn
  14 |    (run mkdir base.tex)
  15 |    (run touch base.tex/base.base.tex))))
  Error: The following both define the same directory target:
  _build/default/base.tex
  - dune:9
  - dune:17
  [1]
